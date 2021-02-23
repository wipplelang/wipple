public enum ValidationResult<T> {
    case valid(T)
    case invalid
}

extension ValidationResult {
    public func map<U>(_ transform: (T) throws -> U) rethrows -> ValidationResult<U> {
        switch self {
        case .valid(let value):
            return try .valid(transform(value))
        case .invalid:
            return .invalid
        }
    }
    
    public func flatMap<U>(_ transform: (T) throws -> ValidationResult<U>) rethrows -> ValidationResult<U> {
        switch self {
        case .valid(let value):
            return try transform(value)
        case .invalid:
            return .invalid
        }
    }
}

public typealias AnyValidationResult = ValidationResult<Any>

public struct Validation<A, B> {
    public var validation: (A, inout Environment, ProgramStack) throws -> ValidationResult<B>
    public var debugLabel: String?
    
    public init(debugLabel: String? = nil, _ validation: @escaping (A, inout Environment, ProgramStack) throws -> ValidationResult<B>) {
        self.debugLabel = debugLabel
        self.validation = validation
    }
    
    public func callAsFunction(_ input: A, _ env: inout Environment, _ stack: ProgramStack) throws -> ValidationResult<B> {
        try self.validation(input, &env, stack)
    }
}

public func || <A, B>(
    lhs: Validation<A, B>,
    rhs: Validation<A, B>
) -> Validation<A, B> {
    Validation(debugLabel: "\(lhs.debugLabel ?? "<validation>") or \(rhs.debugLabel ?? "<validation>")") { a, env, stack in
        switch try lhs(a, &env, stack) {
        case .valid(let b):
            return .valid(b)
        case .invalid:
            return try rhs(a, &env, stack)
        }
    }
}

public func & <A, B, C>(
    lhs: Validation<A, B>,
    rhs: Validation<B, C>
) -> Validation<A, C> {
    Validation(debugLabel: "\(lhs.debugLabel ?? "<validation>") and \(rhs.debugLabel ?? "<validation>")") { a, env, stack in
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
    lhs: Validation<A, B>,
    rhs: Validation<B, C>
) -> Validation<A, (B, C)> {
    Validation(debugLabel: "\(lhs.debugLabel ?? "<validation>") with \(rhs.debugLabel ?? "<validation>")") { a, env, stack in
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

public func toAny<A, B>(_ validation: Validation<A, B>) -> AnyValidation {
    Validation(debugLabel: validation.debugLabel) { value, env, stack in
        switch try validation(value as! A, &env, stack) {
        case .valid(let newValue):
            return .valid(newValue)
        case .invalid:
            return .invalid
        }
    }
}

public func fromAny<A, B>(_ validation: AnyValidation) -> Validation<A, B> {
    Validation(debugLabel: validation.debugLabel) { value, env, stack in
        switch try validation(value, &env, stack) {
        case .valid(let newValue):
            return .valid(newValue as! B)
        case .invalid:
            return .invalid
        }
    }
}
