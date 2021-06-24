import Foundation
import Runtime
import Wipple

public protocol ValueConvertible {
    static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self

    func toValue(_ env: Env, _ stack: Stack) throws -> Value

    static var wippleIdentifiers: [String: String] { get }
}

public extension ValueConvertible {
    static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let typeInfo = try! typeInfo(of: Self.self)

        switch typeInfo.kind {
        case .enum:
            let variant = try value.get(Variant.self, or: "Expected variant", env, stack)

            guard variant.belongs(to: VariantSet(enum: Self.self)) else {
                throw Exit.error("Variant is of incorrect set", stack)
            }

            assert(variant.value == nil)

            return createCLikeEnum(case: self.swiftIdentifier(for: variant.name))
        case .struct:
            let module = try Module.new(value, env, stack)

            return try createInstance { propertyInfo in
                let name = Self.wippleIdentifier(for: propertyInfo.name)

                let variable = try Name(rawValue: name)
                    .resolveIfPresent(module.capturedEnv, stack)

                let propertyType: Any.Type
                if try! Runtime.typeInfo(of: propertyInfo.type).kind == .optional {
                    guard variable != nil else {
                        return nil as Any? as Any
                    }

                    propertyType = try! Runtime.typeInfo(of: propertyInfo.type).genericTypes[0]
                } else {
                    propertyType = propertyInfo.type
                }

                guard let value = variable else {
                    throw Exit.error("Expected value for '\(name)'", stack)
                }

                if let propertyType = propertyType as? Primitive.Type {
                    return try value.get(trait: Trait(propertyType), env, stack).primitiveValue!
                } else if let propertyType = propertyType as? ValueConvertible.Type {
                    return try propertyType.fromValue(value, env, stack)
                } else {
                    fatalError("Default implementation of \(#function) requires property to conform to Primitive or ValueConvertible")
                }
            }
        default:
            fatalError("Default implementation of \(#function) may only be used on struct and enum types")
        }
    }

    func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        let typeInfo = try! typeInfo(of: Self.self)

        switch typeInfo.kind {
        case .enum:
            let name = Self.wippleIdentifier(for: caseName(ofCLikeEnum: self))

            let variant = Variant(in: VariantSet(enum: Self.self), name: name)

            return Value(variant)
        case .struct:
            let module = try Module(capturing: Env.global.child { env in
                for propertyInfo in typeInfo.properties {
                    let property = try! propertyInfo.get(from: self)

                    let value: Value
                    if let property = property as? Primitive {
                        value = Value(property)
                    } else if let property = property as? ValueConvertible {
                        value = try property.toValue(env, stack)
                    } else {
                        fatalError("Default implementation of \(#function) requires property to conform to Primitive or ValueConvertible")
                    }

                    let name = Self.wippleIdentifier(for: propertyInfo.name)

                    try env.setVariable(name, to: value, stack)
                }
            })

            return Value(module)
        default:
            fatalError("Default implementation of \(#function) may only be used on struct and enum types")
        }
    }

    static var wippleIdentifiers: [String: String] {
        Self.defaultWippleIdentifiers()
    }

    fileprivate static func wippleIdentifier(for swiftIdentifier: String) -> String {
        self.wippleIdentifiers[swiftIdentifier] ?? swiftIdentifier
    }

    fileprivate static func swiftIdentifier(for wippleIdentifier: String) -> String {
        self.wippleIdentifiers.lazy.compactMap { $0.value == wippleIdentifier ? $0.key : nil }.first ?? wippleIdentifier
    }

    static func defaultWippleIdentifiers() -> [String: String] {
        let typeInfo = try! typeInfo(of: Self.self)

        let properties = typeInfo.kind == .enum
            ? typeInfo.cases.map(\.name)
            : typeInfo.properties.map(\.name)

        return Dictionary(uniqueKeysWithValues: properties.map { property in
            (property, Self.defaultWippleIdentifier(for: property, kind: typeInfo.kind))
        })
    }

    private static func defaultWippleIdentifier(for swiftIdentifier: String, kind: Kind) -> String {
        swiftIdentifier
            .reduce(into: [""]) { result, character in
                if character.isUppercase {
                    result.append(String(character))
                } else {
                    result[result.count - 1].append(character)
                }
            }
            .map { kind == .enum ? $0.capitalized : $0.lowercased() }
            .joined(separator: "-")
    }
}

public extension VariantSet {
    init<T: ValueConvertible>(enum type: T.Type) {
        let typeInfo = try! typeInfo(of: T.self)
        typeInfo.ensureIsCLikeEnum()

        self.init(id: ID(T.self), variants: Dictionary(uniqueKeysWithValues: typeInfo.cases.map { caseInfo in
            let name = T.wippleIdentifier(for: caseInfo.name)
            return (name, VariantConstructor(name: name, valuePattern: nil))
        }))
    }
}

public extension Pattern {
    init<T: ValueConvertible>(convertible type: T.Type) {
        self.init { value, env, stack in
            try T.fromValue(value, env, stack).toValue(env, stack)
        }
    }
}

// MARK: Builtin ValueConvertible implementations

extension Bool: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let variant = try value.get(Variant.self, or: "Expected boolean", env, stack)

        guard let condition = variant.condition else {
            throw Exit.error("Expected boolean", stack)
        }

        return condition
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Variant(condition: self))
    }
}

extension Int: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).intValue
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension Int8: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).int8Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension Int16: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).int16Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension Int32: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).int32Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension Int64: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).int64Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension UInt: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).uintValue
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension UInt8: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).uint8Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension UInt16: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).uint16Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension UInt32: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).uint32Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension UInt64: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).uint64Value
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension Float: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).floatValue
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(Double(self))))
    }
}

extension Double: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        let decimal = try value.get(Number.self, or: "Expected number", env, stack).rawValue
        return (decimal as NSDecimalNumber).doubleValue
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Number(rawValue: Decimal(self)))
    }
}

extension String: ValueConvertible {
    public static func fromValue(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        try value.get(Text.self, or: "Expected text", env, stack).rawValue
    }

    public func toValue(_ env: Env, _ stack: Stack) throws -> Value {
        Value(Text(rawValue: self))
    }
}
