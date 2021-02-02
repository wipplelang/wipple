import Foundation

/// Conformances define how to derive a trait from a value satisfying a
/// condition
public struct Conformance<A, B> {
    public var derivedTraitID: TraitID<B>
    public var validation: Validation<Value, A>
    public var deriveTraitValue: (A, inout Environment) throws -> B

    public init(derivedTraitID: TraitID<B>, validation: @escaping Validation<Value, A>, deriveTraitValue: @escaping (A, inout Environment) throws -> B) {
        self.derivedTraitID = derivedTraitID
        self.validation = validation
        self.deriveTraitValue = deriveTraitValue
    }
}

public struct AnyConformance {
    public var derivedTraitID: AnyTraitID
    public var validation: Validation<Value, Any>
    public var deriveTraitValue: (Any, inout Environment) throws -> Any
    
    public init<A, B>(_ conformance: Conformance<A, B>) {
        self.derivedTraitID = .init(erasing: conformance.derivedTraitID)
        self.validation = any(conformance.validation)
        self.deriveTraitValue = { input, env in
            try conformance.deriveTraitValue(input as! A, &env)
        }
    }
}

// MARK: - Adding conformances to the environment

public extension Environment {
    mutating func addConformance<A, B>(_ conformance: Conformance<A, B>) {
        self.conformances.append(AnyConformance(conformance))
    }

    mutating func addConformance<A, B>(derivedTraitID: TraitID<B>, validation: @escaping Validation<Value, A>, deriveTraitValue: @escaping (A, inout Environment) throws -> B) {
        self.addConformance(Conformance(
            derivedTraitID: derivedTraitID,
            validation: validation,
            deriveTraitValue: deriveTraitValue
        ))
    }
}
