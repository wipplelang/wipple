public enum ValidationResult<T> {
    case valid(T)
    case invalid
}

public typealias AnyValidationResult = ValidationResult<Any>

public typealias Validation<A, B> = (A, inout Environment, ProgramStack) throws
    -> ValidationResult<B>

public func || <A, B>(
    lhs: @escaping Validation<A, B>,
    rhs: @escaping Validation<A, B>
) -> Validation<A, B> {
    { a, env, stack in
        switch try lhs(a, &env, stack) {
        case .valid(let b):
            return .valid(b)
        case .invalid:
            return try rhs(a, &env, stack)
        }
    }
}

public func & <A, B, C>(
    lhs: @escaping Validation<A, B>,
    rhs: @escaping Validation<B, C>
) -> Validation<A, C> {
    { a, env, stack in
        switch try lhs(a, &env, stack) {
        case .valid(let b):
            switch try rhs(b, &env, stack) {
            case .valid(let c):
                return .valid(c)
            case .invalid:
                return .invalid
            }
        case .invalid:
            return .invalid
        }
    }
}

public func && <A, B, C>(
    lhs: @escaping Validation<A, B>,
    rhs: @escaping Validation<B, C>
) -> Validation<A, (B, C)> {
    { a, env, stack in
        switch try lhs(a, &env, stack) {
        case .valid(let b):
            switch try rhs(b, &env, stack) {
            case .valid(let c):
                return .valid((b, c))
            case .invalid:
                return .invalid
            }
        case .invalid:
            return .invalid
        }
    }
}

public typealias AnyValidation = Validation<Any, Any>

public func toAny<A, B>(_ validation: @escaping Validation<A, B>) -> AnyValidation {
    { value, env, stack in
        switch try validation(value as! A, &env, stack) {
        case .valid(let newValue):
            return .valid(newValue)
        case .invalid:
            return .invalid
        }
    }
}

public func fromAny<A, B>(_ validation: @escaping AnyValidation) -> Validation<A, B> {
    { value, env, stack in
        switch try validation(value, &env, stack) {
        case .valid(let newValue):
            return .valid(newValue as! B)
        case .invalid:
            return .invalid
        }
    }
}
