import Foundation
import Wipple

public extension Env {
    static func stdlib(_ stack: Stack) throws -> Env {
        try Env.global.child { env in
            try setupBuiltins(env, stack)
            try loadPrelude(env, stack)
            try loadFiles(env, stack)
        }
    }
}

private func loadFiles(_ env: Env, _ stack: Stack) throws {
    var stack = stack
    stack.diagnostics.add("Loading standard library")

    for (file, code) in wippleSources {
        let fileEnv = env.child()

        _ = try includeString(code, file: .init(path: URL(fileURLWithPath: file), displayMode: .fileName), fileEnv, stack)
        try env.use(fileEnv, stack)
    }
}

private func loadPrelude(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("#stdlib-link", to: stdlibLinkFunction, stack)

    try env.setVariable("trait", to: traitFunction, stack)

    try env.setVariable("is", to: isFunction, stack)

    try env.setVariable("inline", to: inlineFunction, stack)
    try env.setVariable("new", to: newFunction, stack)
    try env.setVariable("use", to: useFunction, stack)
    try env.setVariable("eval-global!", to: evalGlobalFunction, stack)
    try env.setVariable("use-global!", to: useGlobalFunction, stack)
    try env.setComputedVariable("global?", stack, compute: globalQComputedVariable)

    try env.setVariable("match", to: matchFunction, stack)
    try env.setVariable("loop", to: loopFunction, stack)
    try env.setVariable("return", to: returnFunction, stack)
    try env.setVariable("break", to: breakFunction, stack)

    try env.setVariable("show", to: showFunction, stack)
    try env.setVariable("format", to: formatFunction, stack)

    try env.setVariable("operator!", to: operatorFunction, stack)
    try env.setVariable("precedence!", to: precedenceFunction, stack)
    try env.setVariable("Arity", to: Value(VariantSet(enum: AnyArity.self)), stack)
    try env.setVariable("Associativity", to: Value(VariantSet(enum: Precedence.Associativity.self)), stack)
    try env.setVariable("Direction", to: Value(VariantSet(enum: Operator.Direction.self)), stack)

    let assignmentPrecedence = Env.global.addHighestPrecedence(associativity: .none)
    try Env.global.setVariable("assignment-precedence", to: Value(assignmentPrecedence), stack)
    try Env.global.addOperator(":", assignmentOperator, to: assignmentPrecedence, stack)
    try Env.global.addOperator(":>", computedAssignmentOperator, to: assignmentPrecedence, stack)
    try Env.global.addOperator("==", relationOperator, to: assignmentPrecedence, stack)

    let functionPrecedence = Env.global.addPrecedence(associativity: .left, lowerThan: assignmentPrecedence)
    try Env.global.setVariable("function-precedence", to: Value(functionPrecedence), stack)
    try Env.global.addOperator("->", closureOperator, to: functionPrecedence, stack)
    try Env.global.addOperator("=>", templateOperator, to: functionPrecedence, stack)

    let powerPrecedence = Env.global.addPrecedence(associativity: .right, lowerThan: functionPrecedence)
    try Env.global.setVariable("power-precedence", to: Value(powerPrecedence), stack)
    try Env.global.addOperator("^", powerOperator, to: powerPrecedence, stack)

    let multiplicationPrecedence = Env.global.addPrecedence(associativity: .left, lowerThan: powerPrecedence)
    try Env.global.setVariable("multiplication-precedence", to: Value(multiplicationPrecedence), stack)
    try Env.global.addOperator("*", multiplicationOperator, to: multiplicationPrecedence, stack)
    try Env.global.addOperator("/", divisionOperator, to: multiplicationPrecedence, stack)
    try Env.global.addOperator("mod", moduloOperator, to: multiplicationPrecedence, stack)

    let additionPrecedence = Env.global.addPrecedence(associativity: .left, lowerThan: multiplicationPrecedence)
    try Env.global.setVariable("addition-precedence", to: Value(additionPrecedence), stack)
    try Env.global.addOperator("+", additionOperator, to: additionPrecedence, stack)
    try Env.global.addOperator("-", subtractionOperator, to: additionPrecedence, stack)

    let convertPrecedence = Env.global.addPrecedence(associativity: .left, lowerThan: additionPrecedence)
    try Env.global.setVariable("convert-precedence", to: Value(convertPrecedence), stack)
    try Env.global.addOperator("as", asOperator, to: convertPrecedence, stack)
    try Env.global.addOperator("as?", asQOperator, to: convertPrecedence, stack)
    try Env.global.addOperator("is?", isQOperator, to: convertPrecedence, stack)
    try Env.global.addOperator("into", intoOperator, to: convertPrecedence, stack)

    let comparisonPrecedence = Env.global.addPrecedence(associativity: .left, lowerThan: convertPrecedence)
    try Env.global.setVariable("comparison-precedence", to: Value(comparisonPrecedence), stack)
    try Env.global.addOperator("=", equalOperator, to: comparisonPrecedence, stack)
    try Env.global.addOperator("<", lessThanOperator, to: comparisonPrecedence, stack)
}

// MARK: Link values to standard library

private let stdlibLinkFunction = Value(Function { value, env, stack in
    let name = try value.get(Name.self, or: "Expected name", env, stack).rawValue
    let value = try value.evaluate(env, stack)

    Env.global.linkedValues[name] = value

    return .empty
})

// MARK: Trait functions

private let traitFunction = Value(Function { value, env, stack in
    let pattern = try value
        .evaluate(env, stack)
        .get(Pattern.self, or: "Expected pattern", env, stack)

    return Value(Trait(pattern: pattern))
})

// MARK: Pattern functions

private let isFunction = Value(Function { value, env, stack in
    let pattern = try value
        .evaluate(env, stack)
        .get(Pattern.self, or: "Expected pattern", env, stack)

    return Value(Pattern(is: pattern))
})

// MARK: Module functions

private let inlineFunction = Value(Function { value, env, stack in
    let block = try value.get(Block.self, or: "Expected block", env, stack)
    return try block.reduce(location: value.location, env, stack)
})

public extension Module {
    @_disfavoredOverload
    static func new(_ value: Value, _ env: Env, _ stack: Stack) throws -> Value {
        if value.isPrimitive(ofType: Module.self) {
            return value
        }

        let block = try value.get(Block.self, or: "Expected block", env, stack)
        return try Module.evaluateBlock(block, value, env, stack)
    }

    static func new(_ value: Value, _ env: Env, _ stack: Stack) throws -> Self {
        try self.new(value, env, stack).primitiveValue as! Module
    }
}

private let newFunction = Value(Function(rawValue: Module.new))

private let useFunction = Value(Function { value, env, stack in
    let module = try value
        .evaluate(env, stack)
        .get(Module.self, or: "Expected module", env, stack)

    try env.use(module.capturedEnv, stack)

    return .empty
})

private let evalGlobalFunction = Value(Function { value, _, stack in
    try value.evaluate(.global, stack)
})

private let useGlobalFunction = Value(Function { value, env, stack in
    let module = try value
        .evaluate(env, stack)
        .get(Module.self, or: "Expected module", env, stack)

    try Env.global.use(module.capturedEnv, stack)

    return .empty
})

private let globalQComputedVariable: Variable.Compute = { env, _ in
    Value(Variant(condition: env.isGlobal))
}

// MARK: Control flow

private let matchFunction = Value(Function { value, env, stack in
    let valueToMatch = try value.evaluate(env, stack)

    return Value(Function { value, env, stack in
        let block = try value.get(Block.self, or: "Expected block", env, stack)

        var patterns: [(pattern: Wipple.Pattern, name: String?, returnValue: [Value])] = []

        let matchEnv = env.child()

        matchEnv.handleAssignment = { left, right, env, stack in
            let pattern: Wipple.Pattern
            let name: String?

            switch left.count {
            case 1:
                pattern = try left[0]
                    .evaluate(env, stack)
                    .get(Pattern.self, or: "Expected pattern", env, stack)

                name = nil
            case 2:
                pattern = try left[0]
                    .evaluate(env, stack)
                    .get(Pattern.self, or: "Expected pattern", env, stack)

                name = try left[1]
                    .get(Name.self, or: "Expected name", env, stack)
                    .rawValue
            default:
                throw Exit.error("Expected pattern to match and optional name", stack)
            }

            guard env === matchEnv else {
                throw Exit.error("Cannot partially apply assignment inside 'match'", stack)
            }

            patterns.append((pattern, name, right))
        }

        try block.reduce(location: value.location, matchEnv, stack)

        for (pattern, name, returnValue) in patterns {
            guard let matchedValue = try pattern.match(valueToMatch, env, stack) else {
                continue
            }

            if let name = name {
                try matchEnv.setVariable(name, to: matchedValue, stack)
            }

            return try Value(returnValue).evaluate(matchEnv, stack)
        }

        throw Exit.error("Value did not match any pattern", stack)
    })
})

private let loopFunction = Value(Function { value, env, stack in
    while true {
        try cancelIfSignaled(stack)

        do {
            _ = try value.evaluate(env, stack)
        } catch let exit as Exit {
            if case let .break(value, _) = exit {
                return value
            } else {
                throw exit
            }
        }
    }
})

private func makeExitFunction(_ exit: @escaping (Value, Stack) -> Exit) -> Value {
    Value(Function(transparent: true) { value, env, stack in
        let value = try value.evaluate(env, stack)
        throw exit(value, stack)
    })
}

private let returnFunction = makeExitFunction(Exit.return)
private let breakFunction = makeExitFunction(Exit.break)

// MARK: Output functions

public typealias Show = (Value, Env, Stack) throws -> Void

public extension Stack {
    private struct ShowKey: StackKey {
        typealias Value = Show
    }

    var show: Show {
        get {
            self[ShowKey.self] ?? { _, _, stack in
                throw Exit.error("Cannot use 'show' because this runtime does not handle output", stack)
            }
        }
        set { self[ShowKey] = newValue }
    }
}

private let showFunction = Value(Function { value, env, stack in
    try stack.show(value, env, stack)

    return .empty
})

private let formatFunction = Value(Function { value, env, stack in
    let formatText = try value
        .evaluate(env, stack)
        .get(Text.self, or: "Expected format text", env, stack)

    func buildFormatter(index: Int, remainingStrings: ArraySlice<String>, result: String) -> Value {
        if remainingStrings.count == 1 {
            return Value(Text(rawValue: result + remainingStrings.first!))
        }

        return Value(Function { value, env, stack in
            let leadingString = remainingStrings.first!
            let remainingStrings = remainingStrings.dropFirst()

            let value = try value.evaluate(env, stack)

            let text = try value.format(env, stack)

            return buildFormatter(
                index: index - 1,
                remainingStrings: remainingStrings,
                result: result + leadingString + text
            )
        })
    }

    let strings = formatText.rawValue
        .split(separator: "_", omittingEmptySubsequences: false)
        .map(String.init)

    return buildFormatter(
        index: strings.count - 1,
        remainingStrings: strings[...],
        result: ""
    )
})

// MARK: Operator functions

private struct OperatorDeclaration: ValueConvertible {
    struct Arity: ValueConvertible {
        let left: AnyArity
        let right: AnyArity
    }

    let precedence: Precedence
    let arity: Arity
    let direction: Operator.Direction
    let function: Function
}

private extension Operator {
    static func erased(_ declaration: OperatorDeclaration) -> Operator? {
        func singleValue<Arity: _Arity>(_ arity: Arity, _ parsed: Arity.Parsed) -> Value? {
            switch arity {
            case is NoneArity:
                return nil
            case is SingleArity:
                return (parsed as! Value)
            case is ManyArity:
                return Value(List(rawValue: parsed as! [Value]))
            default:
                fatalError("Custom types may not conform to Arity")
            }
        }

        func oper<Left: _Arity, Right: _Arity>(leftArity: Left, rightArity: Right) -> Operator {
            Operator(leftArity: leftArity, rightArity: rightArity, direction: declaration.direction) { left, right, env, stack in
                let left = singleValue(leftArity, left)
                let right = singleValue(rightArity, right)

                if left == nil || right == nil {
                    return try declaration.function(left ?? right!, env, stack)
                } else {
                    let function = try declaration.function(left!, env, stack)
                        .get(Function.self, or: "Expected function for right side of operator", env, stack)

                    return try function(right!, env, stack)
                }
            }
        }

        switch (declaration.arity.left, declaration.arity.right) {
        case (.none, .none):
            return nil
        case (.none, .single):
            return oper(leftArity: Arity.none, rightArity: Arity.single)
        case (.none, .many):
            return oper(leftArity: Arity.none, rightArity: Arity.many)
        case (.single, .none):
            return oper(leftArity: Arity.single, rightArity: Arity.none)
        case (.many, .none):
            return oper(leftArity: Arity.many, rightArity: Arity.none)
        case (.single, .single):
            return oper(leftArity: Arity.single, rightArity: Arity.single)
        case (.single, .many):
            return oper(leftArity: Arity.single, rightArity: Arity.many)
        case (.many, .single):
            return oper(leftArity: Arity.many, rightArity: Arity.single)
        case (.many, .many):
            return oper(leftArity: Arity.many, rightArity: Arity.many)
        }
    }
}

private let operatorFunction = Value(Function { value, env, stack in
    let operatorName = try value
        .evaluate(env, stack)
        .get(Name.self, or: "Expected name", env, stack)
        .rawValue

    return Value(Function { value, env, stack in
        let declaration = try OperatorDeclaration.fromValue(value, env, stack)

        guard let oper = Operator.erased(declaration) else {
            throw Exit.error("Operator must accept at least one value on the left or right side", stack)
        }

        try Env.global.addOperator(operatorName, oper, to: declaration.precedence, stack)

        return .empty
    })
})

private struct PrecedenceDeclaration: ValueConvertible {
    let associativity: Precedence.Associativity
    unowned let higherThan: Precedence?
    unowned let lowerThan: Precedence?
}

private let precedenceFunction = Value(Function { value, env, stack in
    let precedenceName = try value
        .get(Name.self, or: "Expected name", env, stack)
        .rawValue

    return Value(Function { value, env, stack in
        let declaration = try PrecedenceDeclaration.fromValue(value, env, stack)

        guard (declaration.higherThan == nil) != (declaration.lowerThan == nil) else {
            throw Exit.error("Must specify either higher-than or lower-than in precedence declaration", stack)
        }

        let prec: Precedence
        if let higherThan = declaration.higherThan {
            prec = Env.global.addPrecedence(associativity: declaration.associativity, higherThan: higherThan)
        } else {
            prec = Env.global.addPrecedence(associativity: declaration.associativity, lowerThan: declaration.lowerThan!)
        }

        try! Env.global.setVariable(precedenceName, to: Value(prec), stack)

        return .empty
    })
})

// MARK: Assignment operators

private func makeAssignmentOperator(using assign: KeyPath<Env, HandleAssignment>) -> Operator {
    Operator(leftArity: Arity.many, rightArity: Arity.many) { left, right, env, stack in
        try env[keyPath: assign](left, right, env, stack)

        return .empty
    }
}

private var assignmentOperator: Operator {
    makeAssignmentOperator(using: \.handleAssignment)
}

private var computedAssignmentOperator: Operator {
    makeAssignmentOperator(using: \.handleComputedAssignment)
}

private var relationOperator: Operator {
    Operator(leftArity: Arity.many, rightArity: Arity.many) { left, right, env, stack in
        let fromTrait: Trait
        let name: String?

        switch left.count {
        case 1:
            fromTrait = try Value(left)
                .evaluate(env, stack)
                .get(Trait.self, or: "Expected trait", env, stack)

            name = nil
        case 2:
            fromTrait = try left[0]
                .evaluate(env, stack)
                .get(Trait.self, or: "Expected trait", env, stack)

            name = try left[1]
                .get(Name.self, or: "Expected name", env, stack)
                .rawValue
        default:
            throw Exit.error("Expected predicate for relation", stack)
        }

        guard right.count == 2 else {
            throw Exit.error("Expected a value to derive and its trait", stack)
        }

        let toTrait = try right[0]
            .evaluate(env, stack)
            .get(Trait.self, or: "Expected trait", env, stack)

        let derivedValue = right[1]

        try env.addRelation(from: fromTrait, to: toTrait, stack) { value, stack in
            let deriveEnv = env.child()

            if let name = name {
                try deriveEnv.setVariable(name, to: value, stack)
            }

            return try derivedValue.evaluate(deriveEnv, stack)
        }

        return .empty
    }
}

// MARK: Function operators

private var closureOperator: Operator {
    Operator(leftArity: Arity.many, rightArity: Arity.many) { left, right, env, stack in
        Value(Closure(
            capturing: env,
            parameter: try getParameter(left, env, stack),
            returning: Value(right)
        ))
    }
}

private var templateOperator: Operator {
    Operator(leftArity: Arity.many, rightArity: Arity.many) { left, right, env, stack in
        Value(Template(
            capturing: env,
            parameters: try left.map { parameter in
                let parsed: ManyArity.Parsed
                if let list = parameter.primitiveValue as? List {
                    parsed = list.rawValue
                } else {
                    parsed = [parameter]
                }

                return try getParameter(parsed, env, stack)
            },
            returning: Value(right)
        ))
    }
}

private func getParameter(_ input: [Value], _ env: Env, _ stack: Stack) throws -> (pattern: Wipple.Pattern, name: String) {
    let pattern: Wipple.Pattern
    let name: String

    switch input.count {
    case 1:
        pattern = .any

        name = try Value(input)
            .get(Name.self, or: "Closure parameter must be a name", env, stack)
            .rawValue
    case 2:
        pattern = try input[0]
            .evaluate(env, stack)
            .get(Pattern.self, or: "Expected pattern", env, stack)

        name = try input[1]
            .get(Name.self, or: "Closure parameter must be a name", env, stack)
            .rawValue
    default:
        throw Exit.error("Expected parameter", stack)
    }

    return (pattern, name)
}

// MARK: Math operators

private func makeMathOperator(_ operation: @escaping (Value) -> (Value, Env, Stack) throws -> Value) -> Operator {
    Operator(leftArity: Arity.single, rightArity: Arity.single) { left, right, env, stack in
        let left = try left.evaluate(env, stack)
        return try operation(left)(right, env, stack)
    }
}

private var powerOperator: Operator {
    makeMathOperator(Value.power)
}

private var multiplicationOperator: Operator {
    makeMathOperator(Value.multiply)
}

private var divisionOperator: Operator {
    makeMathOperator(Value.divide)
}

private var moduloOperator: Operator {
    makeMathOperator(Value.modulo)
}

private var additionOperator: Operator {
    makeMathOperator(Value.add)
}

private var subtractionOperator: Operator {
    makeMathOperator(Value.subtract)
}

private var equalOperator: Operator {
    makeMathOperator(Value.equal)
}

private var lessThanOperator: Operator {
    makeMathOperator(Value.lessThan)
}

// MARK: Conversion operators

private var asOperator: Operator {
    Operator(leftArity: Arity.single, rightArity: Arity.single) { left, right, env, stack in
        let value = try left.evaluate(env, stack)

        let pattern = try right
            .evaluate(env, stack)
            .get(Pattern.self, or: "Expected pattern", env, stack)

        guard let matchedValue = try pattern.match(value, env, stack) else {
            throw Exit.error("Invalid value", stack)
        }

        return matchedValue
    }
}

private var asQOperator: Operator {
    Operator(leftArity: Arity.single, rightArity: Arity.single) { left, right, env, stack in
        let value = try left.evaluate(env, stack)

        let pattern = try right
            .evaluate(env, stack)
            .get(Pattern.self, or: "Expected pattern", env, stack)

        let matchedValue = try pattern.match(value, env, stack)

        return Value(Variant(maybe: matchedValue))
    }
}

private var isQOperator: Operator {
    Operator(leftArity: Arity.single, rightArity: Arity.single) { left, right, env, stack in
        let value = try left.evaluate(env, stack)

        let pattern = try right
            .evaluate(env, stack)
            .get(Pattern.self, or: "Expected pattern", env, stack)

        let isValid = try pattern.match(value, env, stack) != nil

        return Value(Variant(condition: isValid))
    }
}

private var intoOperator: Operator {
    Operator(leftArity: Arity.single, rightArity: Arity.single) { left, right, env, stack in
        let value = try left.evaluate(env, stack)

        let trait = try right
            .evaluate(env, stack)
            .get(Trait.self, or: "Expected trait", env, stack)

        let traitValue = try value.get(trait: trait, or: "Cannot use this value to represent this trait", env, stack)

        return Value(trait: trait, value: traitValue)
    }
}
