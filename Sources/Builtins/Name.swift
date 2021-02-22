public struct Name {
    public var name: String
    public var location: SourceLocation?

    public init(
        _ name: String,
        location: SourceLocation? = nil
    ) {
        self.name = name
        self.location = location
    }
}

extension TraitID where T == Name {
    public static var name: Self {
        .builtin("Name")
    }
}

extension Trait where T == Name {
    public static func name(_ value: Name) -> Self {
        Trait(id: .name) { _, _ in value }
    }
}

public typealias AssignFunc = (Value, inout Environment, ProgramStack) throws -> Void

extension TraitID where T == AssignFunc {
    public static var assign: Self {
        .builtin("assign")
    }
}

extension Trait where T == AssignFunc {
    public static func assign(_ value: @escaping AssignFunc) -> Self {
        Trait(id: .assign) { _, _ in value }
    }
}

extension TraitID where T == () {
    public static var computed: Self {
        .builtin("Computed")
    }
}

extension Trait where T == () {
    public static func computed() -> Self {
        Trait(id: .computed) { _, _ in }
    }
}

internal func setupName(_ env: inout Environment) {
    // Name : trait
    env.variables["Name"] = Value(
        .traitConstructor(TraitConstructor(id: .name, validation: toAny(TraitID.name.validation)))
    )

    // Name ::= Assign
    env.addConformance(
        Conformance(
            derivedTraitID: .assign,
            validation: TraitID.name.validation,
            deriveTraitValue: { name, _, _ in
                { value, env, stack in
                    let value = try value.evaluate(&env, stack)
                    env.variables[name.name] = value
                }
            }
        )
    )

    // Name ::= Evaluate
    env.addConformance(
        Conformance(
            derivedTraitID: .evaluate,
            validation: TraitID.name.validation,
            deriveTraitValue: { name, _, _ in
                { env, stack in
                    let stack = stack.add("Resolving variable \(name.name)")

                    guard let variable = env.variables[name.name] else {
                        throw ProgramError(message: "Name does not refer to a variable", stack)
                    }

                    return try variable.hasTrait(.computed, &env, stack)
                        ? variable.evaluate(&env, stack)
                        : variable
                }
            }
        )
    )

    // Name ::= Macro-Parameter
    env.addConformance(
        Conformance(
            derivedTraitID: .macroParameter,
            validation: TraitID.name.validation,
            deriveTraitValue: { name, _, _ in
                { value, env, stack in
                    let parameter: MacroParameter = name.name
                    let replacement = try value.evaluate(&env, stack)

                    return (parameter, replacement)
                }
            }
        )
    )

    // Name ::= Macro-Expand
    env.addConformance(
        Conformance(
            derivedTraitID: .macroExpand,
            validation: TraitID.name.validation,
            deriveTraitValue: { name, _, _ in
                { parameter, replacement, _, _ in
                    name.name == parameter
                        ? replacement
                        : Value(.name(name))
                }
            }
        )
    )

    // Name ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.name.validation,
            deriveTraitValue: { name, _, _ in
                Text(name.name)
            }
        )
    )
}
