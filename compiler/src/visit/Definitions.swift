public struct Defined: Fact {
    public let definition: Definition

    public func render(into context: RenderContext) { context.string("is a definition") }
}

public class Definition {
    public let node: Node
    public let name: String?
    public let comments: [String]

    public init(node: Node, name: String?, comments: [String]) {
        self.node = node
        self.name = name
        self.comments = comments
    }
}

public class VariableDefinition: Definition {
    public let value: Node
    public let isMutable: Bool

    public init(node: Node, name: String, value: Node, isMutable: Bool) {
        self.value = value
        self.isMutable = isMutable
        super.init(node: node, name: name, comments: [])
    }
}

public class ConstantDefinition: Definition {
    public struct Attributes {
        public let unit: Bool
        public let connect: [ConnectionAttributeValueSyntax]

        static func parse(_ attributes: [Node], db: DB) -> Self {
            .init(
                unit: parseAttribute(attributes, named: "unit", db: db),
                connect: parseAttributes(attributes, named: "connect", db: db),
            )
        }
    }

    public let attributes: Attributes
    public var value: Node?

    public init(node: Node, name: String, comments: [String], attributes: Attributes) {
        self.attributes = attributes
        super.init(node: node, name: name, comments: comments)
    }
}

public class TypeDefinition: Definition {
    public struct Attributes {
        public let intrinsic: Bool
        public let representation: String?
        public let abi: String?

        static func parse(_ attributes: [Node], db: DB) -> Self {
            .init(
                intrinsic: parseAttribute(attributes, named: "intrinsic", db: db),
                representation: (parseAttribute(
                    attributes,
                    named: "representation",
                    value: StringAttributeValueSyntax.self,
                    db: db,
                )?
                .value)
                .map(String.init),
                abi: (parseAttribute(
                    attributes,
                    named: "abi",
                    value: StringAttributeValueSyntax.self,
                    db: db,
                )?
                .value)
                .map(String.init),
            )
        }
    }

    public let attributes: Attributes
    public let parameters: [Node]

    public init(
        node: Node,
        name: String,
        comments: [String],
        attributes: Attributes,
        parameters: [Node],
    ) {
        self.attributes = attributes
        self.parameters = parameters
        super.init(node: node, name: name, comments: comments)
    }
}

public class TraitDefinition: Definition {
    public struct Attributes {
        static func parse(_ attributes: [Node], db: DB) -> Self { .init() }
    }

    public let attributes: Attributes
    public let parameters: [Node]

    public init(
        node: Node,
        name: String,
        comments: [String],
        attributes: Attributes,
        parameters: [Node],
    ) {
        self.attributes = attributes
        self.parameters = parameters
        super.init(node: node, name: name, comments: comments)
    }
}

public class InstanceDefinition: Definition {
    public struct Attributes {
        public let `default`: Bool
        public let error: Bool

        static func parse(_ attributes: [Node], db: DB) -> Self {
            .init(
                default: parseAttribute(attributes, named: "default", db: db),
                error: parseAttribute(attributes, named: "error", db: db),
            )
        }
    }

    public let attributes: Attributes
    public var value: Node?
    public let traitNode: Node

    public init(node: Node, comments: [String], attributes: Attributes, traitNode: Node) {
        self.attributes = attributes
        self.traitNode = traitNode
        super.init(node: node, name: nil, comments: comments)
    }
}

public class TypeParameterDefinition: Definition {
    public init(node: Node, name: String?) { super.init(node: node, name: name, comments: []) }
}

public class MarkerConstructorDefinition: Definition {}

public class StructureConstructorDefinition: Definition {
    public let fields: [(String, Node)]

    public init(node: Node, name: String, comments: [String], fields: [(String, Node)]) {
        self.fields = fields
        super.init(node: node, name: name, comments: comments)
    }
}

public class VariantConstructorDefinition: Definition {
    public let typeDefinition: Node
    public let variant: Node
    public let index: Int
    public let elements: [Node]

    public init(
        node: Node,
        name: String,
        typeDefinition: Node,
        variant: Node,
        index: Int,
        elements: [Node],
    ) {
        self.typeDefinition = typeDefinition
        self.variant = variant
        self.index = index
        self.elements = elements
        super.init(node: node, name: name, comments: [])
    }
}
