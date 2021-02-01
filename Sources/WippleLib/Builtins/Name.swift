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
        try Trait.value(.name, in: self, &env)
    }

    func nameValueIfPresent(_ env: inout Environment) throws -> Name? {
        try Trait.value(.name, ifPresentIn: self, &env)
    }
}

// MARK: - Assign

public typealias AssignFunction = (Value, inout Environment) throws -> Void

extension Trait.ID {
    static let assign = Self(debugLabel: "Assign")
}

public extension Trait {
    static func assign(_ assign: @escaping AssignFunction) -> Trait {
        Trait(id: .assign) { _ in
            assign
        }
    }
}

public extension Value {
    func assignValue(_ env: inout Environment) throws -> AssignFunction {
        try Trait.value(.assign, in: self, &env)
    }

    func assignValueIfPresent(_ env: inout Environment) throws -> AssignFunction? {
        try Trait.value(.assign, ifPresentIn: self, &env)
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
    env.variables["Name"] = Value.new(.traitConstructor(.name, validation: Trait.validation(for: .name)))

    // Name ::= Assign
    env.addConformance(
        derivedTraitID: .assign,
        validation: Trait.validation(for: .name),
        deriveTraitValue: { value, env -> AssignFunction in
            let name = value as! Name

            return { value, env in
                env.variables[name] = value
            }
        }
    )

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
        derivedTraitID: .text,
        validation: Trait.validation(for: .name),
        deriveTraitValue: { value, env in
            value as! Name
        }
    )
}
