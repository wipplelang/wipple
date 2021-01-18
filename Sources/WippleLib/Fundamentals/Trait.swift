import Foundation

/// Traits uniquely describe how a value can be used in the program
/// (representation or behavior)
public struct Trait: Hashable {
    public var id: Self.ID
    public var value: (inout Environment) throws -> Any

    public init(id: Self.ID, value: @escaping (inout Environment) throws -> Any) {
        self.id = id
        self.value = value
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(self.id)
    }

    @available(*, deprecated: 0, message: "== only compares the trait IDs, not their values")
    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }
}

public extension Trait {
    struct ID: Hashable, CustomStringConvertible {
        public var id: UUID
        public var debugLabel: String?

        public init(debugLabel: String? = nil) {
            self.id = UUID()
            self.debugLabel = debugLabel
        }

        public var description: String {
            self.debugLabel ?? self.id.description
        }
    }
}

// MARK: - Adding trait to value

public extension Value {
    static func assoc(_ trait: Trait) -> Value {
        Value().trait(trait)
    }

    func trait(_ trait: Trait) -> Value {
        var value = self
        value.traits.insert(trait)
        return value
    }
}

// MARK: - Finding traits

public extension Trait {
    /// Find a trait in a value if present or derivable via a conformance
    static func find(_ id: Self.ID, ifPresentIn value: Value, _ env: inout Environment) throws -> Trait? {
        if let trait = value.traits.first(where: { $0.id == id }) {
            return trait
        }

        let derivedTraits: [Trait] = try env.conformances.compactMap { conformance in
            if conformance.derivedTraitID == id, case let .valid(validatedValue) = try conformance.validation(value, &env) {
                return conformance.deriveTrait(from: validatedValue, &env)
            }

            return nil
        }

        if derivedTraits.count > 1 {
            throw ProgramError("Value satisfies multiple conformances deriving this trait, so the trait to derive is ambiguous")
        }

        return derivedTraits.first
    }

    /// Find a trait in a value, failing if not present or derivable via a
    /// conformance
    static func find(_ id: Self.ID, in value: Value, _ env: inout Environment) throws -> Trait {
        guard let trait = try self.find(id, ifPresentIn: value, &env) else {
            throw ProgramError("Cannot find trait")
        }

        return trait
    }

    /// Check whether a trait is present in a value or derivable via a
    /// conformance
    static func check(_ id: Self.ID, isPresentIn value: Value, _ env: inout Environment) throws -> Bool {
        try self.find(id, ifPresentIn: value, &env) != nil
    }

    static func validation(for id: Self.ID) -> Validation {
        return { value, env in
            guard let trait = try Trait.find(id, ifPresentIn: value as! Value, &env) else {
                return .invalid
            }

            return .valid(newValue: try trait.value(&env))
        }
    }
}
