import Foundation

public enum TraitID<T>: Hashable {
    case builtin(String)
    case runtime(UUID)
}

extension TraitID: Equatable {
    public static func == <U>(lhs: TraitID<T>, rhs: TraitID<U>) -> Bool {
        switch (lhs, rhs) {
        case (.builtin(let a), .builtin(let b)):
            return a == b
        case (.runtime(let a), .runtime(let b)):
            return a == b
        default:
            return false
        }
    }
}

public typealias AnyTraitID = TraitID<Any>

extension AnyTraitID {
    public init<T>(
        _ id: TraitID<T>
    ) {
        switch id {
        case .builtin(let id):
            self = .builtin(id)
        case .runtime(let id):
            self = .runtime(id)
        }
    }
}

extension TraitID {
    public init(
        fromAny id: AnyTraitID
    ) {
        switch id {
        case .builtin(let id):
            self = .builtin(id)
        case .runtime(let id):
            self = .runtime(id)
        }
    }
}

public struct Trait<T> {
    public var id: TraitID<T>
    public var value: (inout Environment, ProgramStack) throws -> T

    public init(
        id: TraitID<T>,
        value: @escaping (inout Environment, ProgramStack) throws -> T
    ) {
        self.id = id
        self.value = value
    }
}

extension Trait: Equatable {
    public static func == <U>(lhs: Trait<T>, rhs: Trait<U>) -> Bool {
        lhs.id == rhs.id
    }
}

extension Trait: Hashable {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(self.id)
    }
}

public typealias AnyTrait = Trait<Any>

extension AnyTrait {
    public init<T>(
        _ trait: Trait<T>
    ) {
        self.init(id: AnyTraitID(trait.id), value: trait.value)
    }
}

extension Trait {
    public init(
        fromAny trait: AnyTrait
    ) {
        self.init(id: TraitID(fromAny: trait.id)) { env, stack in
            try trait.value(&env, stack) as! T
        }
    }
}

extension Value {
    public init<T>(
        _ trait: Trait<T>
    ) {
        self.init(traits: [AnyTrait(trait)])
    }

    public func add<T>(_ trait: Trait<T>) -> Value {
        var value = self
        value.traits.insert(AnyTrait(trait))
        return value
    }
}

extension TraitID {
    public var validation: Validation<Value, T> {
        { value, env, stack in
            guard
                let trait = try value.getTraitIfPresent(
                    self,
                    &env,
                    stack.add("Validating \(value.format(&env, stack))")
                )
            else {
                return .invalid
            }

            return .valid(trait)
        }
    }
}

extension Value {
    public func getTrait<T>(
        _ id: TraitID<T>,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> T {
        try self.getTrait(id, orError: "Cannot find trait", &env, stack)
    }

    public func getTrait<T>(
        _ id: TraitID<T>,
        orError errorMessage: @autoclosure () -> String,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> T {
        let stack = stack.add("Finding trait for '\(self.format(&env, stack))'")

        guard
            let trait = try self.findTrait(
                id,
                &env,
                stack
            )
        else {
            throw ProgramError(message: errorMessage(), stack)
        }

        return try trait.value(&env, stack)
    }

    public func getTraitIfPresent<T>(
        _ id: TraitID<T>,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> T? {
        let stack = stack.add("Finding trait for '\(self.format(&env, stack))'")

        return try self.findTrait(id, &env, stack)?.value(&env, stack)
    }

    public func hasTrait<T>(
        _ id: TraitID<T>,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> Bool {
        let stack = stack.add("Finding trait for '\(self.format(&env, stack))'")

        return try self.findTrait(id, &env, stack) != nil
    }

    private func findTrait<T>(
        _ id: TraitID<T>,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> Trait<T>? {
        if let trait = self.traits.first(where: { $0.id == id }) {
            return Trait(fromAny: trait)
        }

        var derivedTrait: Trait<T>?

        for conformance in env.conformances {
            guard conformance.derivedTraitID == id else { continue }

            guard case .valid(let validatedValue) = try conformance.validation(self, &env, stack)
            else {
                continue
            }

            if derivedTrait != nil {
                throw ProgramError(
                    message:
                        "Value satisfies multiple conformances deriving this trait, so the trait to derive is ambiguous",
                    stack
                )
            }

            var capturedEnv = env

            derivedTrait = Trait(id: id) { _, stack in
                try conformance.deriveTraitValue(validatedValue, &capturedEnv, stack) as! T
            }
        }

        return derivedTrait
    }
}
