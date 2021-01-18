import Foundation

public enum ValidationResult {
    case valid(newValue: Any)
    case invalid
}

public typealias Validation = (Any, inout Environment) throws -> ValidationResult

public func && (lhs: @escaping Validation, rhs: @escaping Validation) -> Validation {
    return { value, env in
        switch try lhs(value, &env) {
        case let .valid(newValue):
            return try rhs(newValue, &env)
        case .invalid:
            return .invalid
        }
    }
}

public func || (lhs: @escaping Validation, rhs: @escaping Validation) -> Validation {
    return { value, env in
        switch try lhs(value, &env) {
        case let .valid(newValue):
            return .valid(newValue: newValue)
        case .invalid:
            return try rhs(value, &env)
        }
    }
}
