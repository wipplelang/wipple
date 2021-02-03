import Foundation

public typealias Name = String

extension TraitID where T == Name {
    static let name = Self(debugLabel: "Name")
}

public extension Trait {
    static func name(_ name: Name) -> Trait<Name> {
        .init(id: .name) { _ in
            name
        }
    }
}

// MARK: - Assign

public typealias AssignFunction = (Value, inout Environment) throws -> Void

extension TraitID where T == AssignFunction {
    static let assign = Self(debugLabel: "Assign")
}

public extension Trait {
    static func assign(_ assign: @escaping AssignFunction) -> Trait<AssignFunction> {
        .init(id: .assign) { _ in
            assign
        }
    }
}

// MARK: - Computed

extension TraitID where T == Void {
    static let computed = Self(debugLabel: "Computed")
}

public extension Trait {
    static func computed() -> Trait<Void> {
        .init(id: .computed) { _ in
            Void()
        }
    }
}

// MARK: - Initialize

func initializeName(_ env: inout Environment) {
    // Name : trait
    env.variables["Name"] = Value.new(.traitConstructor(.name, validation: TraitID.name.validation()))

    // Name ::= Assign
    env.addConformance(
        derivedTraitID: .assign,
        validation: TraitID.name.validation(),
        deriveTraitValue: { name, env in
            return { value, env in
                env.variables[name] = value
            }
        }
    )

    // Name ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: TraitID.name.validation(),
        deriveTraitValue: { name, env -> EvaluateFunction in
            return { env in
                guard var variable = env.variables[name] else {
                    throw ProgramError("No such variable")
                }

                if try variable.hasTrait(.computed, &env) {
                    variable = try variable.evaluate(&env)
                }

                return variable
            }
        }
    )
    
    // Name ::= Macro-Parameter
    env.addConformance(
        derivedTraitID: .macroParameter,
        validation: TraitID.name.validation(),
        deriveTraitValue: { name, env in
            return { input, env in
                return (
                    name: name,
                    replacement: try input.evaluate(&env)
                )
            }
        }
    )
    
    // Name ::= Macro-Expand
    env.addConformance(
        derivedTraitID: .macroExpand,
        validation: TraitID.name.validation(),
        deriveTraitValue: { name, env in
            return { parameter, replacement, env in
                name == parameter
                    ? replacement
                    : Value.new(.name(name))
            }
        }
    )

    // Name ::= Display
    env.addConformance(
        derivedTraitID: .text,
        validation: TraitID.name.validation(),
        deriveTraitValue: { value, env in
            value
        }
    )
}
