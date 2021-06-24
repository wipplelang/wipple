import Foundation
import Wipple

public func run(_ code: String, file: SourceFile? = nil, setup: RunSetup) throws -> Error? {
    do {
        let (env, stack) = try setup()

        let program = try loadString(code, file: file, stack)
        _ = try importProgram(program, file: file, in: env, stack)
    } catch let exit as Exit {
        return exit.error
    }

    return nil
}

@discardableResult
public func run(_ code: String, file: SourceFile? = nil, setup: RunSetup.Then? = nil) throws -> [RunOutput] {
    var output: [RunOutput] = []

    let setup = RunSetup {
        try defaultSetup(onOutput: { output.append($0) })
    }

    if let error = try run(code, file: file, setup: setup) {
        output.append(.error(error))
    }

    return output
}

@_disfavoredOverload
@discardableResult
public func run(_ code: String, file: SourceFile? = nil, setup: RunSetup.Then? = nil) throws -> String {
    try run(code, file: file)
        .map(\.description)
        .joined(separator: "\n")
}

public enum RunOutput: CustomStringConvertible {
    case log(String)
    case error(Error)

    public var components: (success: Bool, message: String) {
        switch self {
        case .log(let message):
            return (success: true, message)
        case .error(let error):
            return (success: false, error.description)
        }
    }

    public var description: String {
        switch self {
        case .log(let text):
            return text
        case .error(let error):
            return error.description
        }
    }
}

public struct RunSetup: RawRepresentable {
    public typealias Then = (Env, inout Stack) throws -> Void

    public let rawValue: () throws -> (Env, Stack)

    public init(rawValue: @escaping () throws -> (Env, Stack)) {
        self.rawValue = rawValue
    }

    public func callAsFunction() throws -> (Env, Stack) {
        try self.rawValue()
    }

    public func then(perform: @escaping Then) -> RunSetup {
        RunSetup {
            var (env, stack) = try self()
            try perform(env, &stack)
            return (env, stack)
        }
    }
}

public func defaultSetup(onOutput: @escaping (RunOutput) -> Void) throws -> (Env, Stack) {
    let env = Env.global
    var stack = Stack()

    env.clear()
    try env.use(.stdlib(stack), stack)

    stack.show = { value, env, stack in
        let text = try value
            .evaluate(env, stack)
            .format(env, stack)

        onOutput(.log(text))
    }

    return (env, stack)
}
