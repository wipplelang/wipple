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

    let assignmentOperator = Operator(
        arity: .variadic { left, right, env in
            func group(_ list: List) -> Value {
                list.count == 1 ? list[0] : Value.assoc(.list(list))
            }

            guard let assign = try group(left).assignValueIfPresent(&env) else {
                throw ProgramError("Cannot assign to this value because it does not have the Assign trait")
            }

            let right = try group(right).evaluate(&env)

            try assign(right, &env)

            return Value()
        },
        associativity: .right
    )

    try env.registerOperator(assignmentOperator, precedence: .lowest)
    env.variables[":"] = Value.assoc(.operator(assignmentOperator))
}
