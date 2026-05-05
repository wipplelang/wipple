import Foundation

public struct Layer: Sendable {
    public let name: String
    public let files: [any Visitable]

    public init(name: String, files: [any Visitable]) {
        self.name = name
        self.files = files
    }
}

public func readFile(db: DB, path: String, source: String) -> any Visitable {
    do {
        let parser = try Parser(path: URL(filePath: path).lastPathComponent, source: source)
        let file = try parseFile(with: parser)
        try parser.finish()
        return file
    } catch { return ParseErrorSyntax(span: error.span, error: error) }
}

public func readLayer(db: DB, path directoryPath: String) throws -> Layer {
    let files = try FileManager.default.contentsOfDirectory(atPath: directoryPath)
        .map { URL(filePath: directoryPath).appendingPathComponent($0).path }
        .filter { filePath in
            FileManager.default.isReadableFile(atPath: filePath) && filePath.hasSuffix(".wipple")
        }
        .sorted()

    return Layer(
        name: directoryPath,
        files: try files.map { path in
            let source = try String(contentsOfFile: path, encoding: .utf8)
            return readFile(db: db, path: path, source: source)
        },
    )
}
