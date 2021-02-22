public func setup() throws -> Environment {
    var env = Environment()

    setupBlock(&env)
    setupEmpty(&env)
    setupEvaluate(&env)
    setupList(&env)
    setupMacro(&env)
    setupName(&env)
    setupOperator(&env)
    setupQuoted(&env)
    setupTraitConstructor(&env)
    setupValidationContainer(&env)

    // TODO: Load bundled prelude files
    temporaryPrelude(&env)

    return env
}

// TODO: Temporary
private func temporaryPrelude(_ env: inout Environment) {
    // 'new' function

    func add(
        _ base: Value,
        _ traitConstructor: TraitConstructor,
        _ value: Value,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> Value {
        let validatedValue: Any
        switch try traitConstructor.validation(value, &env, stack) {
        case .valid(let value):
            validatedValue = value
        case .invalid:
            throw ProgramError(message: "Cannot use this value to represent this trait", stack)
        }

        let trait = Trait(id: traitConstructor.id) { _, _ in validatedValue }

        return base.add(trait)
    }

    env.variables["new"] = Value(
        .function { value, env, stack in
            let traitConstructor = try value.evaluate(&env, stack)
                .getTrait(.traitConstructor, &env, stack)

            return Value(
                .function { value, env, stack in
                    try add(.empty, traitConstructor, value.evaluate(&env, stack), &env, stack)
                }
            )
        }
    )

    // 'do' function

    env.variables["do"] = Value(
        .function { value, env, stack in
            var innerEnv = env
            return try value.evaluate(&innerEnv, stack)
        }
    )

    // Assignment operator (::)

    func group(_ list: [Value]) -> Value {
        return list.count == 1
            ? list[0]
            : Value(.list(List(list)))
    }

    func assign(
        left: Value,
        right: Value,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> Value {
        let stack = stack.add(
            "Assigning '\(right.format(&env, stack))' to '\(left.format(&env, stack))'"
        )

        let assign = try left.getTrait(
            .assign,
            orError: "Cannot assign to this value becasue it does not have the Assign trait",
            &env,
            stack
        )

        try assign(right, &env, stack)

        return .empty
    }

    let assignmentPrecedenceGroup: VariadicPrecedenceGroup = env.addPrecedenceGroup(
        associativity: .right,
        .highest
    )

    let assignmentOperator = VariadicOperator { left, right, env, stack in
        try assign(left: group(left), right: group(right).evaluate(&env, stack), &env, stack)
    }

    env.addOperator(assignmentOperator, in: assignmentPrecedenceGroup)
    env.variables[":"] = Value(.operator(.variadic(assignmentOperator)))

    // Add trait operator (::)

    let addTraitOperator = VariadicOperator { left, right, env, stack in
        let value = group(left)

        // TODO: Auto-derive a value for the trait using the conformance if only
        // the trait is provided
        if right.count != 2 {
            throw ProgramError(message: "Expected a trait and a value for the trait", stack)
        }

        let traitConstructorValue = try right[0].evaluate(&env, stack)
        let traitValue = try right[1].evaluate(&env, stack)

        let stack = stack.add(
            "Adding trait '\(traitConstructorValue.format(&env, stack))' with '\(traitValue.format(&env, stack))' to '\(value.format(&env, stack))'"
        )

        let traitConstructor = try traitConstructorValue.getTrait(.traitConstructor, &env, stack)

        let newValue = try add(value, traitConstructor, traitValue, &env, stack)

        return try assign(
            left: value,
            // We have to quote the result because we've already evaluated it;
            // in real Wipple code, the result would be assigned to a variable
            // before being passed here and we wouldn't have this problem
            right: Value(.quoted(Quoted(newValue))),
            &env,
            stack
        )
    }

    env.addOperator(addTraitOperator, in: assignmentPrecedenceGroup)
    env.variables["::"] = Value(.operator(.variadic(addTraitOperator)))

    // Macro operator (=>)

    let functionPrecedenceGroup = env.addPrecedenceGroup(
        associativity: .right,
        .lowerThan(assignmentPrecedenceGroup)
    )

    let macroOperator = VariadicOperator { left, right, env, stack in
        let defineParameter = try group(left)
            .getTrait(
                .macroParameter,
                orError: "Macro parameter must have the Macro-Parameter trait",
                &env,
                stack
            )

        return Value(
            .macro(
                Macro(
                    defineParameter: defineParameter,
                    valueToExpand: group(right)
                )
            )
        )
    }

    env.addOperator(macroOperator, in: functionPrecedenceGroup)
    env.variables["=>"] = Value(.operator(.variadic(macroOperator)))

    // Closures

    let closureOperator = VariadicOperator { left, right, env, stack in
        let defineParameter = try group(left)
            .getTrait(.assign, orError: "Closure parameter must have the Assign trait", &env, stack)

        let returnValue = group(right)

        var closureEnv = env

        return Value(
            .function { value, _, stack in
                try defineParameter(value, &closureEnv, stack)
                return try returnValue.evaluate(&closureEnv, stack)
            }
        )
    }

    env.addOperator(closureOperator, in: functionPrecedenceGroup)
    env.variables["=>"] = Value(.operator(.variadic(closureOperator)))

    // Math (temporary)

    // TODO: Implement using traits
    func math(
        _ operation: @escaping (Double, Double) -> Double,
        name: String,
        in precedenceGroup: BinaryPrecedenceGroup
    ) {
        let op = BinaryOperator { left, right, env, stack in
            let left = try left.evaluate(&env, stack).getTrait(.number, &env, stack)
            let right = try right.evaluate(&env, stack).getTrait(.number, &env, stack)

            let result = Number(operation(left.number, right.number))

            return Value(.number(result))
        }

        env.addOperator(op, in: precedenceGroup)
        env.variables[name] = Value(.operator(.binary(op)))
    }

    let additionPrecedenceGroup: BinaryPrecedenceGroup = env.addPrecedenceGroup(
        associativity: .left,
        .lowest
    )

    math(+, name: "+", in: additionPrecedenceGroup)
    math(-, name: "-", in: additionPrecedenceGroup)

    let multiplicationPrecedenceGroup = env.addPrecedenceGroup(
        associativity: .left,
        .higherThan(additionPrecedenceGroup)
    )

    math(*, name: "*", in: multiplicationPrecedenceGroup)
    math(/, name: "/", in: multiplicationPrecedenceGroup)
}
