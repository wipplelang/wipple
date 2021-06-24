import Wipple

public struct Literal: RawRepresentable, Primitive {
    public var rawValue: Value

    public init(rawValue: Value) {
        self.rawValue = rawValue
    }
}

public struct Escaped: RawRepresentable, Primitive {
    public var rawValue: Value

    public init(rawValue: Value) {
        self.rawValue = rawValue
    }
}

// FIXME: Split into two different traits
public struct Interpolate: Primitive {
    public typealias RawValue = (_ direct: Bool, Env, Stack) throws -> Value

    public let rawValue: RawValue

    public init(_ rawValue: @escaping RawValue) {
        self.rawValue = rawValue
    }

    public func callAsFunction(direct: Bool, _ env: Env, _ stack: Stack) throws -> Value {
        try self.rawValue(direct, env, stack)
    }
}

public extension Value {
    func interpolate(direct: Bool, _ env: Env, _ stack: Stack) throws -> Value {
        if let interpolate = try self.getIfPresent(Interpolate.self, env, stack) {
            return try interpolate(direct: direct, env, stack)
        } else if direct {
            return try self.evaluate(env, stack)
        } else {
            return self
        }
    }
}

internal func setupLiteral(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Literal", to: Value(Trait(Literal.self)), stack)

    // Literal == Evaluate
    try env.addRelation(from: Literal.self, to: Evaluate.self, stack) { literal in
        Evaluate { env, stack in
            try literal.rawValue.interpolate(direct: false, env, stack)
        }
    }

    // Literal == Text
    try env.addRelation(from: Literal.self, to: Text.self, stack) { literal, _, stack in
        try Text(rawValue: "'\(literal.rawValue.format(env, stack))")
    }

    // literal : 'x -> x (TODO: Write in Wipple code)
    try env.setVariable("literal", to: Value(Function { value, _, _ in value }), stack)

    try env.setVariable("Escaped", to: Value(Trait(Escaped.self)), stack)

    // Escaped == Evaluate
    try env.addRelation(from: Escaped.self, to: Evaluate.self, stack) { escaped in
        Evaluate { env, stack in
            try escaped.rawValue.interpolate(direct: true, env, stack)
        }
    }

    // Escaped == Interpolate
    try env.addRelation(from: Escaped.self, to: Interpolate.self, stack) { escaped in
        Interpolate { _, env, stack in
            try escaped.rawValue.evaluate(env, stack)
        }
    }

    // Escaped == Text
    try env.addRelation(from: Escaped.self, to: Text.self, stack) { escaped, _, stack in
        try Text(rawValue: "\\\(escaped.rawValue.format(env, stack))")
    }

    try env.setVariable("Interpolate", to: Value(Trait(Interpolate.self)), stack)
}
