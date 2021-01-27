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

//    let assignmentOperator = Operator(arity: .variadic, associativity: .right) { value, env in
//        fatalError("TODO")
//    }

    let assignmentOperator = Operator(
        arity: .variadic { left, right, env in
            fatalError("TODO")
        },
        associativity: .right
    )

    try env.registerOperator(assignmentOperator, precedence: .lowest)
    env.variables[":"] = Value.assoc(.operator(assignmentOperator))
}
