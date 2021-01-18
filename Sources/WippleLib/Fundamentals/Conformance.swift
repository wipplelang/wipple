import Foundation

/// Conformances define how to derive a trait from a value satisfying a
/// condition
public struct Conformance {
    public var derivedTraitID: Trait.ID
    public var validation: Validation
    public var deriveTraitValue: (Any, inout Environment) throws -> Any

    public init(derivedTraitID: Trait.ID, validation: @escaping Validation, deriveTraitValue: @escaping (Any, inout Environment) throws -> Any) {
        self.derivedTraitID = derivedTraitID
        self.validation = validation
        self.deriveTraitValue = deriveTraitValue
    }
}

// MARK: - Adding conformances to the environment

public extension Environment {
    mutating func addConformance(_ conformance: Conformance) {
        self.conformances.append(conformance)
    }

    mutating func addConformance(derivedTraitID: Trait.ID, validation: @escaping Validation, deriveTraitValue: @escaping (Any, inout Environment) throws -> Any) {
        self.addConformance(Conformance(
            derivedTraitID: derivedTraitID,
            validation: validation,
            deriveTraitValue: deriveTraitValue
        ))
    }
}

// MARK: - Derive traits

public extension Conformance {
    func deriveTrait(from value: Any, _ env: inout Environment) -> Trait {
        var capturedEnv = env

        return Trait(id: self.derivedTraitID) { _ in
            try self.deriveTraitValue(value, &capturedEnv)
        }
    }
}
