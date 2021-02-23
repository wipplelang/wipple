public struct Conformance<A, B> {
    public var derivedTraitID: TraitID<B>
    public var validation: Validation<Value, A>
    public var deriveTraitValue: (A, inout Environment, ProgramStack) throws -> B

    public init(
        derivedTraitID: TraitID<B>,
        validation: Validation<Value, A>,
        deriveTraitValue: @escaping (A, inout Environment, ProgramStack) throws -> B
    ) {
        self.derivedTraitID = derivedTraitID
        self.validation = validation
        self.deriveTraitValue = deriveTraitValue
    }
}

public typealias AnyConformance = Conformance<Any, Any>

extension AnyConformance {
    public init<A, B>(
        _ conformance: Conformance<A, B>
    ) {
        self
            .init(
                derivedTraitID: AnyTraitID(conformance.derivedTraitID),
                validation: Validation(debugLabel: conformance.validation.debugLabel) { value, env, stack in
                    switch try conformance.validation(value, &env, stack) {
                    case .valid(let value):
                        return .valid(value)
                    case .invalid:
                        return .invalid
                    }
                },
                deriveTraitValue: { value, env, stack in
                    try conformance.deriveTraitValue(value as! A, &env, stack)
                }
            )
    }
}

extension Conformance {
    public init(
        fromAny conformance: AnyConformance
    ) {
        self.init(
            derivedTraitID: TraitID(fromAny: conformance.derivedTraitID),
            validation: Validation(debugLabel: conformance.validation.debugLabel) { value, env, stack in
                switch try conformance.validation(value, &env, stack) {
                case .valid(let value):
                    return .valid(value as! A)
                case .invalid:
                    return .invalid
                }
            },
            deriveTraitValue: { value, env, stack in
                try conformance.deriveTraitValue(value, &env, stack) as! B
            }
        )
    }
}

extension Environment {
    public mutating func addConformance<A, B>(_ conformance: Conformance<A, B>) {
        self.conformances.append(AnyConformance(conformance))
    }
}

extension Conformance: CustomDebugStringConvertible {
    public var debugDescription: String {
        "\(self.validation.debugLabel ?? "<validation>") ::= \(self.derivedTraitID.debugLabel ?? "<trait>")"
    }
}
