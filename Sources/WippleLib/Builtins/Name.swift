import Foundation

public typealias Name = String

extension Trait.ID {
    static let name = Self(debugLabel: "Name")
}

public extension Trait {
    static func name(_ name: Name) -> Trait {
        Trait(id: .name) { _ in
            name
        }
    }
}

public extension Value {
    func nameValue(_ env: inout Environment) throws -> Name {
        try Trait.find(.name, in: self, &env).value(&env) as! Name
    }
}

// MARK: - Computed

extension Trait.ID {
    static let computed = Self(debugLabel: "Computed")
}

public extension Trait {
    static func computed() -> Trait {
        Trait(id: .computed) { _ in
            Void()
        }
    }
}

public extension Value {
    func isComputed(_ env: inout Environment) throws -> Bool {
        try Trait.check(.computed, isPresentIn: self, &env)
    }
}

// MARK: - Initialize

func initializeName(_ env: inout Environment) {
    // Name : trait
    env.variables["Name"] = Value.assoc(.traitConstructor(.name) { value, env in
        throw ProgramError("Cannot explicitly define the Name trait on a value")
    })

    // Name ::= Evaluate
    env.addConformance(
        derivedTraitID: .evaluate,
        validation: Trait.validation(for: .name),
        deriveTraitValue: { value, env -> EvaluateFunction in
            let name = value as! Name

            return { env in
                guard var variable = env.variables[name] else {
                    throw ProgramError("No such variable")
                }

                if try variable.isComputed(&env) {
                    variable = try variable.evaluate(&env)
                }

                return variable
            }
        }
    )

    // Name ::= Display
    env.addConformance(
        derivedTraitID: .display,
        validation: Trait.validation(for: .name),
        deriveTraitValue: { value, env in
            value as! Name
        }
    )
}
