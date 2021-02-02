import Foundation

public enum ValidationResult<T> {
    case valid(newValue: T)
    case invalid

    var isValid: Bool {
        switch self {
        case .valid:
            return true
        case .invalid:
            return false
        }
    }

    func isValid(equalTo value: T) -> Bool where T: Equatable {
        guard case let .valid(newValue) = self else {
            return false
        }

        return newValue == value
    }
}

public typealias Validation<A, B> = (A, inout Environment) throws -> ValidationResult<B>

public typealias AnyValidation = Validation<Any, Any>

public func any<A, B>(_ validation: @escaping Validation<A, B>) -> AnyValidation {
    return { input, env in
        let result = try validation(input as! A, &env)
        
        switch result {
        case .valid(let newValue):
            return .valid(newValue: newValue)
        case .invalid:
            return .invalid
        }
    }
}

public func && <A, B, C> (lhs: @escaping Validation<A, B>, rhs: @escaping Validation<B, C>) -> Validation<A, C> {
    return { value, env in
        switch try lhs(value, &env) {
        case let .valid(newValue):
            return try rhs(newValue, &env)
        case .invalid:
            return .invalid
        }
    }
}

public func || <A, B> (lhs: @escaping Validation<A, B>, rhs: @escaping Validation<A, B>) -> Validation<A, B> {
    return { value, env in
        switch try lhs(value, &env) {
        case let .valid(newValue):
            return .valid(newValue: newValue)
        case .invalid:
            return try rhs(value, &env)
        }
    }
}
