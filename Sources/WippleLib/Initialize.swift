import Foundation

public func initialize(_ env: inout Environment) throws {
    initializeBlock(&env)
    initializeEmpty(&env)
    initializeEnvironmentContainer(&env)
    initializeEvaluation(&env)
    initializeList(&env)
    initializeName(&env)
    initializeNumber(&env)
    initializeQuoted(&env)
    initializeTraitConstructor(&env)

    // TODO: TEMPORARY!!! (implement these in Wipple code)

    // MARK: - Assignment Operator (:)

    func group(_ list: List) -> Value {
        list.count == 1 ? list[0] : Value.new(.list(list))
    }

    func assign(_ left: [Value], _ right: (inout Environment) throws -> Value, _ env: inout Environment) throws -> Value {
        guard let assign = try group(left).assignValueIfPresent(&env) else {
            throw ProgramError("Cannot assign to this value because it does not have the Assign trait")
        }

        let right = try right(&env).evaluate(&env)

        try assign(right, &env)

        return Value()
    }

    let assignmentOperator = Operator(
        arity: .variadic { left, right, env in
            try assign(left, { _ in group(right) }, &env)
        },
        associativity: .right
    )

    try env.registerOperator(assignmentOperator, precedence: .lowest)
    env.variables[":"] = Value.new(.operator(assignmentOperator))

    // MARK: - Trait Operator (::)

    let traitOperator = Operator(
        arity: .variadic { left, right, env in
            try assign(left, { env in
                guard right.count == 2 else {
                    throw ProgramError("Expected a trait and a value for the trait")
                }

                let traitConstructor = try right[0].evaluate(&env).traitConstructorValue(&env)
                let value = try new(traitConstructor, right[1].evaluate(&env), &env)

                return value
            }, &env)
        },
        associativity: .right
    )

    try env.registerOperator(traitOperator, precedence: .sameAs(assignmentOperator))
    env.variables["::"] = Value.new(.operator(traitOperator))

    // MARK: - 'new' Function

    func new(_ traitConstructor: TraitConstructor, _ value: Value, _ env: inout Environment) throws -> Value {
        guard case let .valid(value) = try traitConstructor.validation(value, &env) else {
            throw ProgramError("Cannot use this value to represent this trait")
        }

        return Value.new(Trait(id: traitConstructor.id, value: { _ in value }))
    }

    env.variables["new"] = Value.new(.call { input, env in
        let traitConstructor = try input.traitConstructorValue(&env)

        return Value.new(.call { input, env in
            try new(traitConstructor, input, &env)
        })
    })
}
