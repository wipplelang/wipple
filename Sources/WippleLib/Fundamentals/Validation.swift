import Foundation

public enum ValidationResult {
    case valid(newValue: Any)
    case invalid

    var isValid: Bool {
        switch self {
        case .valid:
            return true
        case .invalid:
            return false
        }
    }

    func isValid<T: Equatable>(equalTo value: T) -> Bool {
        guard case let .valid(newValue) = self else {
            return false
        }

        return (newValue as! T) == value
    }
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
