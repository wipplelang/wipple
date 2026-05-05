import OrderedCollections

public enum IR {
    public struct Program {
        public var files: [Span] = []
        public var topLevel = IR.Definition()
        public var definitions: OrderedDictionary<IR.DefinitionKey, IR.Definition> = [:]
    }

    public struct DefinitionKey: Hashable {
        public var node: Node
        public var substitutions: OrderedDictionary<Node, IR.`Type`>
        public var bounds: OrderedDictionary<Node, IR.Instance>
    }

    public struct Definition {
        public var type: IR.`Type`?
        public var instructions: [IR.Instruction] = []
        public var types: OrderedDictionary<Node, IR.`Type`> = [:]
    }

    public indirect enum `Type`: Hashable {
        case named(
            definition: Node,
            parameters: [IR.`Type`],
            intrinsic: Bool = false,
            representation: String? = nil,
            abi: String? = nil,
        )
        case tuple(elements: [IR.`Type`])
        case function(inputs: [IR.`Type`], output: IR.`Type`)
        case parameter(definition: Node)
    }

    public enum TypeRepresentation: Hashable {
        case intrinsic
        case marker
        case structure([IR.`Type`])
        case enumeration([[IR.`Type`]])
    }

    public enum Instance: Hashable {
        case bound(Node)
        case definition(IR.DefinitionKey)
    }

    public enum Instruction {
        case `if`(
            node: Node?,
            branches: [(conditions: [IR.Condition], instructions: [IR.Instruction], result: Node?)],
            elseBranch: (instructions: [IR.Instruction], result: Node?)?,
        )
        case `return`(value: Node)
        case trace(location: Node)
        case value(node: Node, value: IR.Value)

        func forEachNode(traverseFunctions: Bool = true, perform action: (Node) throws -> Void)
            rethrows
        {
            switch self {
            case .if(let node, let branches, let elseBranch):
                if let node { try action(node) }

                for branch in branches {
                    for condition in branch.conditions {
                        try condition.forEachNode(perform: action)
                    }

                    for instruction in branch.instructions {
                        try instruction.forEachNode(
                            traverseFunctions: traverseFunctions,
                            perform: action,
                        )
                    }

                    if let result = branch.result { try action(result) }
                }

                if let elseBranch {
                    for instruction in elseBranch.instructions {
                        try instruction.forEachNode(
                            traverseFunctions: traverseFunctions,
                            perform: action,
                        )
                    }

                    if let result = elseBranch.result { try action(result) }
                }
            case .return(let node): try action(node)
            case .trace(_): break
            case .value(let node, let value):
                try action(node)

                if traverseFunctions, case .function(let inputs, _, let instructions) = value {
                    for input in inputs { try action(input) }

                    for instruction in instructions {
                        try instruction.forEachNode(traverseFunctions: true, perform: action)
                    }
                }
            }
        }

        func traverse<E: Error>(with action: (Self) throws(E) -> Self) throws(E) -> Self {
            switch try action(self) {
            case .if(let node, let branches, let elseBranch):
                let branches = try branches.map { (conditions, instructions, result) throws(E) in
                    let instructions = try instructions.map { (instruction) throws(E) in
                        try instruction.traverse(with: action)
                    }
                    return (conditions, instructions, result)
                }

                let elseBranch = try elseBranch.map { (instructions, result) throws(E) in
                    let instructions = try instructions.map { (instruction) throws(E) in
                        try instruction.traverse(with: action)
                    }

                    return (instructions, result)
                }

                return .if(node: node, branches: branches, elseBranch: elseBranch)
            case .return(let value): return .return(value: value)
            case .trace(let location): return .trace(location: location)
            case .value(let node, var value):
                if case .function(let inputs, let captures, let instructions) = value {
                    let instructions = try instructions.map { (instruction) throws(E) in
                        try instruction.traverse(with: action)
                    }

                    value = .function(
                        inputs: inputs,
                        captures: captures,
                        instructions: instructions,
                    )
                }

                return .value(node: node, value: value)
            }
        }
    }

    public enum Value {
        case bound(Node)
        case call(function: Node, inputs: [Node])
        case concat(segments: [(String, Node)], trailing: String)
        case constant(IR.DefinitionKey)
        case function(inputs: [Node], captures: [Node], instructions: [IR.Instruction])
        case field(input: Node, fieldName: String, fieldIndex: Int)
        case tuple([Node])
        case marker
        case mutableVariable(Node)
        case number(String)
        case runtime(name: String, inputs: [Node])
        case string(String)
        case structure([(String, Node)])
        case tupleElement(input: Node, index: Int)
        case unreachable
        case variable(Node)
        case variant(index: Int, elements: [Node])
        case variantElement(input: Node, variant: Int, index: Int)
    }

    public enum Condition {
        case or([[IR.Condition]])
        case equalToNumber(input: Node, value: String)
        case equalToString(input: Node, value: String)
        case equalToVariant(input: Node, variant: Int)
        case initialize(variable: Node, node: Node?, value: IR.Value, mutable: Bool)
        case mutate(input: Node, variable: Node)

        func forEachNode(perform action: (Node) throws -> Void) rethrows {
            switch self {
            case .or(let branches):
                for branch in branches {
                    for condition in branch { try condition.forEachNode(perform: action) }
                }
            case .equalToNumber(let input, _), .equalToString(let input, _),
                .equalToVariant(let input, _):
                try action(input)
            case .initialize(let variable, let node, _, _):
                try action(variable)
                if let node { try action(node) }
            case .mutate(let input, let variable):
                try action(input)
                try action(variable)
            }
        }
    }
}

extension DB {
    func irType(for root: Type) throws(CodegenError) -> IR.`Type` {
        let type =
            switch root {
            case .node(let node): self.type(of: node)
            case .constructed(let type): type
            }

        guard let type else { throw CodegenError("no type for \(root)") }

        switch type.tag {
        case .named(let definition):
            var parameters: [IR.`Type`] = []
            for parameter in type.children { parameters.append(try self.irType(for: parameter)) }

            guard let definition = self[definition, Defined.self]?.definition as? TypeDefinition
            else { throw CodegenError("\(definition) is not a type definition") }

            return .named(
                definition: definition.node,
                parameters: parameters,
                intrinsic: definition.attributes.intrinsic,
                representation: definition.attributes.representation,
                abi: definition.attributes.abi,
            )
        case .function:
            var type = type

            let output = try self.irType(for: type.children.removeFirst())

            var inputs: [IR.`Type`] = []
            for parameter in type.children {
                let type = try self.irType(for: parameter)

                inputs.append(type)
            }

            return .function(inputs: inputs, output: output)
        case .tuple:
            var types: [IR.`Type`] = []
            for element in type.children {
                let type = try self.irType(for: element)

                types.append(type)
            }

            return .tuple(elements: types)
        case .block:
            let output = try self.irType(for: type.children[0])

            return .function(inputs: [], output: output)
        case .parameter(let parameter): return .parameter(definition: parameter)
        }
    }
}

extension IR.`Type` {
    func traverse<E: Error>(with action: (Self) throws(E) -> Self) throws(E) -> Self {
        switch try action(self) {
        case .named(let definition, let parameters, let intrinsic, let representation, let abi):
            .named(
                definition: definition,
                parameters: try parameters.map { (parameter) throws(E) in
                    try parameter.traverse(with: action)
                },
                intrinsic: intrinsic,
                representation: representation,
                abi: abi,
            )
        case .tuple(let elements):
            .tuple(
                elements: try elements.map { (element) throws(E) in
                    try element.traverse(with: action)
                }
            )
        case .function(let inputs, let output):
            .function(
                inputs: try inputs.map { (input) throws(E) in try input.traverse(with: action) },
                output: try output.traverse(with: action),
            )
        case .parameter(let definition): .parameter(definition: definition)
        }
    }

    func substitute(from substitutions: OrderedDictionary<Node, IR.`Type`>) -> Self {
        self.traverse { type in
            if case .parameter(let parameter) = type, let substitution = substitutions[parameter] {
                substitution
            } else {
                type
            }
        }
    }
}

extension DB {
    func irNamedTypeRepresentation(definition node: Node, parameters: [IR.`Type`])
        throws(CodegenError) -> IR.TypeRepresentation
    {
        guard let definition = self[node, Defined.self]?.definition as? TypeDefinition else {
            throw CodegenError("\(node) is not a type definition")
        }

        var substitutions: OrderedDictionary<Node, IR.`Type`> = [:]
        for (parameter, type) in zip(definition.parameters, parameters) {
            substitutions[parameter] = type
        }

        if definition.attributes.intrinsic {
            return .intrinsic
        } else if let fields = self[node, StructureFields.self]?.fields {
            var fieldTypes: [IR.`Type`] = []
            for field in fields {
                let type = try self.irType(for: .node(field))
                fieldTypes.append(type.substitute(from: substitutions))
            }

            return .structure(fieldTypes)
        } else if let variants = self[node, EnumerationVariants.self]?.variants {
            var variantTypes: [[IR.`Type`]] = []
            for variant in variants {
                var elementTypes: [IR.`Type`] = []
                for element in variant.elements {
                    let type = try self.irType(for: .node(element))
                    elementTypes.append(type.substitute(from: substitutions))
                }

                variantTypes.append(elementTypes)
            }

            return .enumeration(variantTypes)
        } else {
            return .marker
        }
    }
}
