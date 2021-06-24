import Foundation
import Wipple

// MARK: Equal

public struct Equal: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func equal(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let equal = try self.get(
            Equal.self,
            or: "Cannot compare this value because it does not have the Equal trait",
            env,
            stack
        )

        return try equal.rawValue(other, env, stack)
    }
}

// MARK: Less Than

public struct LessThan: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func lessThan(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let lessThan = try self.get(
            LessThan.self,
            or: "Cannot compare this value because it does not have the Less-Than trait",
            env,
            stack
        )

        return try lessThan.rawValue(other, env, stack)
    }
}

// MARK: Add

public struct Add: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func add(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let add = try self.get(
            Add.self,
            or: "Cannot add this value because it does not have the Add trait",
            env,
            stack
        )

        return try add.rawValue(other, env, stack)
    }
}

// MARK: Subtract

public struct Subtract: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func subtract(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let subtract = try self.get(
            Subtract.self,
            or: "Cannot subtract this value because it does not have the Subtract trait",
            env,
            stack
        )

        return try subtract.rawValue(other, env, stack)
    }
}

// MARK: Multiply

public struct Multiply: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func multiply(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let multiply = try self.get(
            Multiply.self,
            or: "Cannot multiply this value because it does not have the Multiply trait",
            env,
            stack
        )

        return try multiply.rawValue(other, env, stack)
    }
}

// MARK: Divide

public struct Divide: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func divide(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let divide = try self.get(
            Divide.self,
            or: "Cannot divide this value because it does not have the Divide trait",
            env,
            stack
        )

        return try divide.rawValue(other, env, stack)
    }
}

// MARK: Modulo

public struct Modulo: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func modulo(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let modulo = try self.get(
            Modulo.self,
            or: "Cannot divide this value with remainder because it does not have the Modulo trait",
            env,
            stack
        )

        return try modulo.rawValue(other, env, stack)
    }
}

// MARK: Power

public struct Power: RawRepresentable, Primitive {
    public typealias RawValue = (Value, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }
}

public extension Value {
    func power(_ other: Value, _ env: Env, _ stack: Stack) throws -> Value {
        let power = try self.get(
            Power.self,
            or: "Cannot raise this value to a power because it does not have the Power trait",
            env,
            stack
        )

        return try power.rawValue(other, env, stack)
    }
}

// MARK: - Setup

internal func setupMath(_ env: Env, _ stack: Stack) throws {
    try setupMathOperation(Equal.self, traitName: "Equal", using: ==)
    try setupMathOperation(LessThan.self, traitName: "Less-Than", using: <)

    try setupMathOperation(Add.self, traitName: "Add", using: +)
    try setupMathOperation(Subtract.self, traitName: "Subtract", using: -)
    try setupMathOperation(Multiply.self, traitName: "Multiply", using: *)
    try setupMathOperation(Divide.self, traitName: "Divide", using: ensureNonzero(/, "Cannot divide by zero"))
//    try setupMathOperation(Modulo.self, traitName: "Modulo", using: ensureNonzero(%, "Cannot divide with remainder by zero"))
//    try setupMathOperation(Power.self, traitName: "Power", using: ensureNonzero(pow, "Cannot raise to a power of zero"))

    // MARK: Boilerplate

    func ensureNonzero(_ operation: @escaping (Decimal, Decimal) -> Decimal, _ message: String) -> (Decimal, Decimal, Stack) throws -> Decimal {
        { lhs, rhs, stack in
            guard !rhs.isZero else {
                throw Exit.error(message, stack)
            }

            return operation(lhs, rhs)
        }
    }

    func setupMathOperation<T: Primitive & RawRepresentable>(
        _ trait: T.Type,
        traitName: String,
        using operation: @escaping (Decimal, Decimal) throws -> Bool
    ) throws where T.RawValue == (Value, Env, Stack) throws -> Value {
        try setupMathOperation(trait, traitName: traitName) { lhs, rhs, stack in
            Value(Variant(condition: try operation(lhs, rhs)))
        }
    }

    func setupMathOperation<T: Primitive & RawRepresentable>(
        _ trait: T.Type,
        traitName: String,
        using operation: @escaping (Decimal, Decimal) throws -> Decimal
    ) throws where T.RawValue == (Value, Env, Stack) throws -> Value {
        try setupMathOperation(trait, traitName: traitName) { lhs, rhs, _ in
            try operation(lhs, rhs)
        }
    }

    func setupMathOperation<T: Primitive & RawRepresentable>(
        _ trait: T.Type,
        traitName: String,
        using operation: @escaping (Decimal, Decimal, Stack) throws -> Decimal
    ) throws where T.RawValue == (Value, Env, Stack) throws -> Value {
        try setupMathOperation(trait, traitName: traitName) {
            Value(Number(rawValue: try operation($0, $1, $2)))
        }
    }

    func setupMathOperation<T: Primitive & RawRepresentable>(
        _ trait: T.Type,
        traitName: String,
        using operation: @escaping (Decimal, Decimal, Stack) throws -> Value
    ) throws where T.RawValue == (Value, Env, Stack) throws -> Value {
        try env.setVariable(traitName, to: Value(Trait(T.self)), stack)

        try env.addRelation(from: Number.self, to: T.self, stack) { number in
            T { other, env, stack in
                let other = try other
                    .evaluate(env, stack)
                    .get(Number.self, or: "Expected number", env, stack)

                return try operation(number.rawValue, other.rawValue, stack)
            }!
        }
    }
}
