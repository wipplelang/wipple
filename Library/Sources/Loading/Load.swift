import Foundation
import Wipple
import WippleParser

public extension Stack {
    private struct CurrentFileKey: StackKey {
        typealias Value = URL
    }

    var currentFilePath: URL? {
        get { self[CurrentFileKey.self] }
        set { self[CurrentFileKey.self] = newValue }
    }
}

private var cachedImports: [URL: Module] = [:]

/// Import a file into an environment. If this file has already been imported,
/// it won't be loaded again and the cached module will be returned.
public func importFile(_ file: SourceFile, in env: Env, _ stack: Stack) throws -> Module {
    if let module = cachedImports[file.path] {
        return module
    }

    let program = try loadFile(file, stack)
    let module = try importProgram(program, file: file, in: env, stack)

    cachedImports[file.path] = module

    return module
}

public func importProgram(_ program: Block, file: SourceFile? = nil, in env: Env, _ stack: Stack) throws -> Module {
    var stack = stack
    stack.currentFilePath = file?.path
    stack.diagnostics.add(file?.description.map { "Importing \($0)" } ?? "Importing program")

    let env = env.child()
    try program.reduce(location: .init(file: file), env, stack)

    return Module(capturing: env)
}

public func includeFile(_ file: SourceFile, _ env: Env, _ stack: Stack) throws -> Value {
    var stack = stack
    stack.currentFilePath = file.path

    let program = try loadFile(file, stack)
    return try includeProgram(program, file: file, env, stack)
}

public func includeString(_ code: String, file: SourceFile? = nil, _ env: Env, _ stack: Stack) throws -> Value {
    let program = try loadString(code, file: file, stack)
    return try includeProgram(program, file: file, env, stack)
}

public func includeProgram(_ program: Block, file: SourceFile? = nil, _ env: Env, _ stack: Stack) throws -> Value {
    var stack = stack
    stack.diagnostics.add(file.map { "Including \($0)" } ?? "Including program")

    return try program.reduce(location: .init(file: file), env, stack)
}

/// Load a Wipple file into a value without evaluating it.
public func loadFile(_ file: SourceFile, _ stack: Stack) throws -> Block {
    var stack = stack
    stack.diagnostics.add("Loading file \(file)")

    let code: String
    do {
        code = try String(contentsOf: file.path)
    } catch {
        throw Exit.error("Error loading file: \(error)", stack)
    }

    return try loadString(code, file: file, stack)
}

public func loadString(_ code: String, file: SourceFile? = nil, _ stack: Stack) throws -> Block {
    var stack = stack
    stack.diagnostics.add(file.map { "Parsing \($0)" } ?? "Parsing program")

    let tokens = lex(code, file: file)

    let ast: AST
    do {
        ast = try parseFile(tokens.forParsing())
    } catch let error as ParseError {
        var stack = stack
        stack.diagnostics.add(
            file.map { "Parsing file \($0.path)" } ?? "Parsing input",
            location: error.location
        )

        throw Exit.error("Parse error: \(error.message)", stack)
    }

    return ast.value.primitiveValue as! Block
}
