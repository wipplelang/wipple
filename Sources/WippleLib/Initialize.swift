import Foundation

public func initialize(_ env: inout Environment) throws {
    initializeBlock(&env)
    initializeEmpty(&env)
    initializeEnvironmentContainer(&env)
    initializeEvaluation(&env)
    initializeList(&env)
    initializeMacro(&env)
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
        guard let assign = try group(left).traitIfPresent(.assign, &env) else {
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

                let traitConstructor = try right[0].evaluate(&env).trait(.traitConstructor, &env)
                let value = try new(traitConstructor, right[1].evaluate(&env), &env)

                return value
            }, &env)
        },
        associativity: .right
    )

    try env.registerOperator(traitOperator, precedence: .sameAs(assignmentOperator))
    env.variables["::"] = Value.new(.operator(traitOperator))
    
    // MARK: - Macro Operator (=>)
    
    let macroOperator = Operator(
        arity: .variadic { left, right, env in
            guard let defineParameter = try group(left).traitIfPresent(.macroParameter, &env) else {
                throw ProgramError("Macro parameter must satisfy the Macro-Parameter trait")
            }
                        
            return Value.new(.macro(
                defineParameter: defineParameter,
                valueToExpand: group(right)
            ))
        },
        associativity: .right
    )

    try env.registerOperator(macroOperator, precedence: .lowerThan(assignmentOperator))
    env.variables["=>"] = Value.new(.operator(macroOperator))

    // MARK: - 'new' Function

    func new(_ traitConstructor: AnyTraitConstructor, _ value: Value, _ env: inout Environment) throws -> Value {
        guard case let .valid(value) = try traitConstructor.validation(value, &env) else {
            throw ProgramError("Cannot use this value to represent this trait")
        }

        return Value.new(Trait(id: traitConstructor.id) { _ in value })
    }

    env.variables["new"] = Value.new(.function { input, env in
        let traitConstructor = try input.trait(.traitConstructor, &env)

        return Value.new(.function { input, env in
            try new(traitConstructor, input, &env)
        })
    })
    
    // MARK: - 'do' Function
    
    env.variables["do"] = Value.new(.function { input, env in
        var innerEnv = env
        return try input.evaluate(&innerEnv)
    })
    
    // MARK: - Closures (TODO: write in Wipple code)
    
    typealias DefineClosureParameterFunction = (_ input: Value, inout Environment) throws -> Void
    let ClosureParameter = TraitID<DefineClosureParameterFunction>(debugLabel: "Closure-Parameter")
    
    // Name ::= Closure-Parameter
    env.addConformance(
        derivedTraitID: ClosureParameter,
        validation: TraitID.name.validation(),
        deriveTraitValue: { name, env in
            return { input, env in
                env.variables[name] = try input.evaluate(&env)
            }
        }
    )
    
    // TODO: Closure trait
    
    let closureOperator = Operator(
        arity: .variadic { left, right, env in
            guard let defineParameter = try group(left).traitIfPresent(ClosureParameter, &env) else {
                throw ProgramError("Closure parameter must satisfy the Closure-Parameter trait")
            }
            
            let returnValue = group(right)
            
            var closureEnv = env
            
            return Value.new(.function { input, env in
                try defineParameter(input, &closureEnv)
                
                return try returnValue.evaluate(&closureEnv)
            })
        },
        associativity: .right
    )

    try env.registerOperator(closureOperator, precedence: .sameAs(macroOperator))
    env.variables["->"] = Value.new(.operator(closureOperator))
    
    // MARK: - Math (temporary)
    
    // TODO: Implement using traits
    
    func math(_ name: Name, _ operation: (Decimal, Decimal) -> Decimal) -> Operator {
        let op = Operator(
            arity: .binary { left, right, env in
                guard let left = try left.traitIfPresent(.number, &env), let right = try right.traitIfPresent(.number, &env) else {
                    throw ProgramError("Expected numbers")
                }
                
                return Value.new(.number(left + right))
            },
            associativity: .left
        )

        env.variables[name] = Value.new(.operator(op))
        
        return op
    }
    
    let additionOperator = math("+", +)
    try env.registerOperator(additionOperator, precedence: .lowest)

    // TODO: Read left to right or right to left instead of throwing ambiguity error
    let subtractionOperator = math("-", -)
    try env.registerOperator(subtractionOperator, precedence: .sameAs(additionOperator))
    
    let multiplicationOperator = math("*", *)
    try env.registerOperator(multiplicationOperator, precedence: .higherThan(additionOperator))
    
    let divisionOperator = math("/", /)
    try env.registerOperator(divisionOperator, precedence: .sameAs(multiplicationOperator))
}
