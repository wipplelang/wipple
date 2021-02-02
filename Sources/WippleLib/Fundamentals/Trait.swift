import Foundation

/// Traits uniquely describe how a value can be used in the program
/// (representation or behavior)
public struct Trait<T>: Hashable {
    public var id: TraitID<T>
    public var value: (inout Environment) throws -> T

    public init(id: TraitID<T>, value: @escaping (inout Environment) throws -> T) {
        self.id = id
        self.value = value
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(self.id)
    }

    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }
}

public struct TraitID<T>: Hashable, CustomStringConvertible {
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

// MARK: - Type-erased trait

public typealias AnyTraitID = TraitID<Any>

public struct AnyTrait: Hashable {
    public let id: AnyTraitID
    private let value: (inout Environment) throws -> Any
    
    public init<T>(_ trait: Trait<T>) {
        self.id = AnyTraitID(erasing: trait.id)
        self.value = trait.value
    }
    
    public func value<T>(as type: T.Type, _ env: inout Environment) throws -> T? {
        try self.value(&env) as? T
    }
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(self.id)
    }

    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.id == rhs.id
    }
}

public extension TraitID {
    init(anyTraitID: AnyTraitID) {
        self.id = anyTraitID.id
        self.debugLabel = anyTraitID.debugLabel
    }
}

public extension AnyTraitID {
    init<T>(erasing id: TraitID<T>) {
        self.id = id.id
        self.debugLabel = id.debugLabel
    }
}

// MARK: - Adding trait to value

public extension Value {
    static func new<T>(_ trait: Trait<T>) -> Value {
        Value().add(trait)
    }

    func add<T>(_ trait: Trait<T>) -> Value {
        var value = self
        value.traits.insert(AnyTrait(trait))
        return value
    }
}

// MARK: - Finding traits

public extension TraitID {
    func validation() -> Validation<Value, T> {
        return { value, env in
            guard let trait = try value.findTraitIfPresent(self, &env) else {
                return .invalid
            }

            let traitValue = try trait.value(&env)

            return .valid(newValue: traitValue)
        }
    }
}

public extension Value {
    /// Find a trait in a value if present or derivable via a conformance
    func findTraitIfPresent<T>(_ id: TraitID<T>, _ env: inout Environment) throws -> Trait<T>? {
        if let trait = self.traits.first(where: { $0.id.id == id.id }) {
            return Trait(id: .init(anyTraitID: trait.id)) { env in
                try trait.value(as: T.self, &env)!
            }
        }

        let derivedTraits: [Trait<T>] = try env.conformances.compactMap { conformance in
            guard conformance.derivedTraitID.id == id.id else { return nil }
            
            guard conformance.derivedTraitID.id == id.id, case let .valid(validatedValue) = try conformance.validation(self, &env) else {
                return nil
            }
            
            var capturedEnv = env

            return Trait(id: id) { _ in
                try conformance.deriveTraitValue(validatedValue, &capturedEnv) as! T
            }
        }

        if derivedTraits.count > 1 {
            throw ProgramError("Value satisfies multiple conformances deriving this trait, so the trait to derive is ambiguous")
        }

        return derivedTraits.first
    }

    /// Find a trait in a value, failing if not present or derivable via a
    /// conformance
    func findTrait<T>(_ id: TraitID<T>, _ env: inout Environment) throws -> Trait<T> {
        guard let trait = try self.findTraitIfPresent(id, &env) else {
            throw ProgramError("Cannot find trait")
        }

        return trait
    }

    func trait<T>(_ id: TraitID<T>, _ env: inout Environment) throws -> T {
        try self.findTrait(id, &env).value(&env)
    }

    func traitIfPresent<T>(_ id: TraitID<T>, _ env: inout Environment) throws -> T? {
        try self.findTraitIfPresent(id, &env)?.value(&env)
    }

    /// Check whether a trait is present in a value or derivable via a
    /// conformance
    func hasTrait<T>(_ id: TraitID<T>, _ env: inout Environment) throws -> Bool {
        try self.traitIfPresent(id, &env) != nil
    }
}
