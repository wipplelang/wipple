import Compiler
import JavaScriptKit

@JS class CompileResult {
    let db: DB
    let path: String
    let files: [Node]
    let libFiles: [Node]?

    init(db: DB, path: String, files: [Node], libFiles: [Node]?) {
        self.db = db
        self.path = path
        self.files = files
        self.libFiles = libFiles
    }
}

@JS struct File {
    var path: String
    var code: String
}

private var libraries: [String: CompileResult] = [:]

@JS func registerLibrary(name: String, result: CompileResult) { libraries[name] = result }

@JS func compile(files: [File], libraryName: String? = nil) -> CompileResult? {
    var library: CompileResult?
    if let libraryName {
        guard let lib = libraries[libraryName] else { return nil }

        library = lib
    }

    let db = DB(parent: library?.db)

    let path = files[0].path

    let files = compile(
        db: db,
        files: files.map { readFile(db: db, path: $0.path, source: $0.code) },
    )

    return CompileResult(db: db, path: path, files: files, libFiles: library?.files)
}

@JS func format(_ code: String) -> String? { Compiler.format(code) }
