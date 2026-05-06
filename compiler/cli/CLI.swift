import ArgumentParser
import Compiler
import OrderedCollections
import Synchronization

#if os(Linux)
    @preconcurrency import Glibc
    import Foundation
#else
    import Darwin
    import Foundation
#endif

@main struct CLI: ParsableCommand {
    static let configuration = CommandConfiguration(
        commandName: "wipple",
        subcommands: [CompileCommand.self, RunCommand.self, TestCommand.self, DocCommand.self],
    )
}

struct CompileCommand: ParsableCommand {
    static let configuration = CommandConfiguration(commandName: "compile")

    @Option(name: .short) var output: String?
    @OptionGroup var compileOptions: CompileOptions

    func run() throws {
        try compile(with: self.compileOptions, outputPath: self.output, to: &stderr)
    }
}

struct RunCommand: ParsableCommand {
    static let configuration = CommandConfiguration(commandName: "run")

    @Option(name: .short) var output: String?
    @OptionGroup var compileOptions: CompileOptions

    func run() throws {
        guard let wasm = try compile(with: self.compileOptions, to: &stderr) else { return }

        try runWasm(wasm, path: self.output)
    }
}

struct TestCommand: ParsableCommand {
    static let configuration = CommandConfiguration(commandName: "test")

    @OptionGroup var compileOptions: CompileOptions

    func run() throws { try test(with: self.compileOptions) }
}

struct DocCommand: ParsableCommand {
    static let configuration = CommandConfiguration(commandName: "doc")

    @OptionGroup var compileOptions: CompileOptions

    func run() throws { try doc(with: self.compileOptions) }
}

struct CompileOptions: ParsableArguments {
    @Option(name: .long) var lib: [String] = []
    @Flag var facts = false
    @Option(name: .long) var filterFacts: String?
    @Option(name: .long) var filterFeedback: [String] = []
    @Argument var paths: [String] = []
}

@discardableResult func compile(
    with options: CompileOptions,
    outputPath: String? = nil,
    to out: inout some TextOutputStream,
) throws -> String? {
    guard let (libDb, libFiles) = try setup(with: options, time: true, to: &out) else { return nil }

    let db = DB(parent: libDb)

    var paths: [String] = []
    var files: [any Visitable] = []
    for path in options.paths {
        let file = try readFile(
            db: db,
            path: path,
            source: String(contentsOfFile: path, encoding: .utf8),
        )

        paths.append(path)
        files.append(file)
    }

    let layer = Layer(name: paths.joined(separator: ", "), files: files)

    guard
        let files = compile(
            layer: layer,
            with: options,
            db: db,
            prefix: "Compiling ",
            time: true,
            to: &out,
        )
    else { return nil }

    let program = try codegen(db: db, files: files, libraryFiles: libFiles)

    let wasm = try program.wasm(db: db)

    if let outputPath {
        try wasm.write(to: URL(string: outputPath)!, atomically: true, encoding: .utf8)
    }

    return wasm
}

func test(with options: CompileOptions) throws {
    var out = ""
    guard let (libDb, libFiles) = try setup(with: options, to: &out) else { return }

    let layers = try options.paths.map { path in
        let db = DB(parent: libDb)

        let layer = Layer(
            name: URL(filePath: path).lastPathComponent,
            files: [
                try readFile(
                    db: db,
                    path: path,
                    source: String(contentsOfFile: path, encoding: .utf8),
                )
            ],
        )

        return (db, layer)
    }

    struct RunResult: Encodable {
        let file: String
        let output: String
        let graph: String
    }

    let run = { @Sendable (index: Int, test: (db: DB, layer: Layer)) in
        let filter = defaultFilter(db: test.db)

        var out = ""
        if let files = compile(
            layer: test.layer,
            with: options,
            db: test.db,
            progress: (index, layers.count),
            to: &out,
        ) {
            let program = try codegen(db: test.db, files: files, libraryFiles: libFiles)
            let wasm = try program.wasm(db: test.db)

            print("Output:", to: &out)
            let output = try runWasm(wasm, path: nil, captureOutput: true)!
            print(output, terminator: "", to: &out)

        }

        let graph = test.db.graph(including: OrderedSet(test.db.ownedNodes.lazy.filter(filter)))
            .toDOT()

        return RunResult(file: test.layer.name, output: out, graph: graph)
    }

    let results: [Result<RunResult, Error>?]
    if ProcessInfo.processInfo.environment["WIPPLE_TEST_SEQUENTIAL"] != nil {
        results = layers.enumerated().map { index, test in Result { try run(index, test) } }
    } else {
        let mutex = Mutex([Result<RunResult, Error>?](repeating: nil, count: layers.count))

        DispatchQueue.concurrentPerform(iterations: layers.count) { index in
            let result = Result { try run(index, layers[index]) }
            mutex.withLock { $0[index] = result }
        }

        results = mutex.withLock(\.self)
    }

    print(to: &stderr)

    let outputs = try results.map { result in try result!.get() }

    print(String(data: try JSONEncoder().encode(outputs), encoding: .utf8)!)
}

func doc(with options: CompileOptions) throws {
    guard let (libDb, _) = try setup(with: options, time: true, to: &stderr) else { return }

    let db = DB(parent: libDb)

    var paths: [String] = []
    var files: [any Visitable] = []
    for path in options.paths {
        let file = try readFile(
            db: db,
            path: path,
            source: String(contentsOfFile: path, encoding: .utf8),
        )

        paths.append(path)
        files.append(file)
    }

    let layer = Layer(name: paths.joined(separator: ", "), files: files)

    guard
        compile(
            layer: layer,
            with: options,
            db: db,
            prefix: "Compiling ",
            time: true,
            to: &stderr,
        ) != nil
    else { return }

    struct DocumentationItem: Codable {
        var declaration: String
        var kind: String
        var docs: String
        var imported: Bool
    }

    var items: [String: DocumentationItem] = [:]
    db.forEachFact(Defined.self) { node, _ in
        Queries.documentation(db, node) { documentation in
            guard let name = documentation.name else { return }

            let writer = FeedbackWriter(db: db)
            writer.write(documentation.comments)
            let (docs, _) = writer.finish { $0.markdown(db: db) }

            items[name] = .init(
                declaration: documentation.declaration,
                kind: documentation.kind,
                docs: docs,
                imported: !node.belongs(to: db),
            )
        }
    }

    print(String(data: try JSONEncoder().encode(items), encoding: .utf8)!)
}

@discardableResult func runWasm(_ wasm: String, path: String?, captureOutput: Bool = false) throws
    -> String?
{
    let outputURL =
        path.map(URL.init(fileURLWithPath:))
        ?? FileManager.default.temporaryDirectory.appendingPathComponent(
            "wipple-\(Int32.random(in: (Int32.max >> 1)...Int32.max))"
        )

    #if os(macOS)
        try? FileManager.default.trashItem(at: outputURL, resultingItemURL: nil)
    #else
        try? FileManager.default.removeItem(at: outputURL)
    #endif

    try FileManager.default.createDirectory(at: outputURL, withIntermediateDirectories: true)

    // Copy runtime files
    try Data(PackageResources.package_json)
        .write(to: outputURL.appendingPathComponent("package.json"))
    try Data(PackageResources.index_js).write(to: outputURL.appendingPathComponent("index.js"))
    try Data(PackageResources.runtime_js).write(to: outputURL.appendingPathComponent("runtime.js"))

    try wasm.write(to: outputURL.appending(path: "main.wat"), atomically: true, encoding: .utf8)

    let wasmToolsProcess = Process()
    wasmToolsProcess.launchPath = "/usr/bin/env"
    wasmToolsProcess.currentDirectoryURL = outputURL
    wasmToolsProcess.arguments = [
        "wasm-tools", "parse", "main.wat", "-o", outputURL.appending(path: "main.wasm").path,
    ]
    try wasmToolsProcess.run()
    wasmToolsProcess.waitUntilExit()

    guard wasmToolsProcess.terminationStatus == 0 else { return nil }

    let nodeProcess = Process()
    nodeProcess.launchPath = "/usr/bin/env"
    nodeProcess.arguments = ["node", outputURL.path]
    if captureOutput { nodeProcess.standardOutput = Pipe() }

    try nodeProcess.run()
    nodeProcess.waitUntilExit()

    if path == nil { try FileManager.default.removeItem(at: outputURL) }

    if captureOutput {
        if let data = try (nodeProcess.standardOutput as! Pipe).fileHandleForReading.readToEnd() {
            return String(data: data, encoding: .utf8)
        } else {
            return ""
        }
    } else {
        return nil
    }
}

func setup(with options: CompileOptions, time: Bool = false, to out: inout some TextOutputStream)
    throws -> (DB, [Node])?
{
    let db = DB(debugEnabled: ProcessInfo.processInfo.environment["WIPPLE_DEBUG"] != nil)

    let libLayers = try options.lib.map { try readLayer(db: db, path: $0) }

    var libFiles: [Node] = []
    for layer in libLayers {
        guard
            let files = compile(
                layer: layer,
                with: options,
                db: db,
                prefix: "Compiling ",
                time: time,
                hideFacts: true,
                to: &out,
            )
        else { return nil }

        libFiles.append(contentsOf: files)
    }

    return (db, libFiles)
}

func compile(
    layer: Layer,
    with options: CompileOptions,
    db: DB,
    prefix: String = "",
    time: Bool = false,
    progress: (Int, Int)? = nil,
    hideFacts: Bool = false,
    to out: inout some TextOutputStream,
) -> [Node]? {
    defer { if progress == nil { print(to: &stderr) } }

    if let (index, total) = progress {
        print("\u{001B}[2K\r", terminator: "", to: &stderr)  // reset line
        print("(\(index + 1)/\(total)) ", terminator: "", to: &stderr)
    }

    print("\(prefix)\(layer.name)", terminator: "", to: &stderr)
    if time { print("...", terminator: "", to: &stderr) }

    let clock = ContinuousClock()
    let start = clock.now

    let files = compile(db: db, files: layer.files)

    if time {
        let duration = clock.now - start
        print(" done (\(duration))", terminator: "", to: &stderr)
    }

    if options.facts && !hideFacts {
        let filePaths = layer.files.map(\.span.path)

        let nodeIsInLayer = { (node: Node) in
            if let filter = options.filterFacts {
                if let filter = Int(filter) { return node.id == filter }

                let range = filter.split(separator: "-", maxSplits: 1)
                if range.count == 2, let min = Int(range[0]), let max = Int(range[1]) {
                    return node.id >= min && node.id <= max
                }
            }

            if db.debugEnabled { return true }

            guard let span = db[node, Syntax.self]?.value.span else { return false }

            return filePaths.contains(span.path)
        }

        print("Facts:", to: &out)
        print(db.debugDescription(filter: nodeIsInLayer), to: &out)
    }

    let nodeFilter = defaultFilter(db: db)

    var seenFeedback: [Node: Set<String>] = [:]
    let feedbackItems = collectFeedback(db: db) { item in
        nodeFilter(item.location.primary)
            && (options.filterFeedback.isEmpty || options.filterFeedback.contains(item.id))
            && seenFeedback[item.location.primary, default: []].insert(item.id).inserted
    }

    var feedbackCount = 0
    for item in feedbackItems {
        if feedbackCount == 0 { print("\nFeedback:", to: &out) } else { print(to: &out) }

        let renderContext = RenderContext(db: db)
        renderContext.node(item.location.primary)
        let (location, _) = renderContext.render(with: { $0.markdown(db: db) })

        print("\n\(location) (\(item.id))\n", to: &out)

        let (feedback, _) = item.display(db) { $0.markdown(db: db) }

        for line in feedback.split(separator: "\n", omittingEmptySubsequences: false) {
            print("  \(line)", to: &out)
        }

        feedbackCount += 1
    }

    guard feedbackCount == 0 else { return nil }

    return files
}

private struct Stderr: TextOutputStream {
    nonmutating func write(_ string: String) {
        #if os(Linux)
            fputs(string, Glibc.stderr)
        #else
            fputs(string, Darwin.stderr)
        #endif
    }
}

nonisolated(unsafe) private var stderr = Stderr()
