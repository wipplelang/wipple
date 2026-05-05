import OrderedCollections

public enum Type: Equatable, Renderable {
    case node(Node)
    case constructed(ConstructedType)

    var node: Node? {
        switch self {
        case .node(let node): node
        case .constructed(_): nil
        }
    }

    public func traverse(with action: (Self) -> Self, stack: [Type] = []) -> Self {
        let type = action(self)

        if stack.contains(type) {
            return type  // recursive type
        }

        var stack = stack
        stack.append(type)

        switch type {
        case .node(_): return type
        case .constructed(var type):
            type.children = type.children.map { $0.traverse(with: action, stack: stack) }
            return .constructed(type)
        }
    }

    public var referencedNodes: OrderedSet<Node> {
        var referencedNodes: OrderedSet<Node> = []
        _ = self.traverse { type in
            if case .node(let node) = type { referencedNodes.append(node) }

            return type
        }

        return referencedNodes
    }

    public var referencesNodes: Bool { self.referencesNodes(where: { _ in true }) }

    public func referencesNodes(where predicate: (Node) -> Bool) -> Bool {
        var found = false
        _ = self.traverse { type in
            guard !found, case .node(let node) = type else { return type }

            if predicate(node) { found = true }

            return type
        }

        return found
    }

    func display(db: DB, root: Bool = true) -> String {
        switch self {
        case .node(_): return "_"
        case .constructed(let type):
            let children = type.children.map { child in
                { root in child.display(db: db, root: root) }
            }

            return type.display(db, children, root)
        }
    }

    public func render(into context: RenderContext, root: Bool = true) {
        let description = self.display(db: context.db, root: root)

        let node =
            switch self {
            case .node(let node): node
            case .constructed(let type): type.representative ?? type.definition
            }

        if let node { context.link(description, node) } else { context.code(description) }
    }
}

public struct ConstructedType: Equatable {
    public enum Tag: Equatable {
        case named(Node)
        case function
        case tuple
        case block
        case parameter(Node)
    }

    public typealias Display = (DB, [(_ root: Bool) -> String], _ root: Bool) -> String

    public var representative: Node?
    public var tag: Tag
    public var children: [Type]
    public var display: Display

    public init(_ tag: Tag, children: [Type], display: @escaping Display) {
        self.tag = tag
        self.children = children
        self.display = display
    }

    public static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.tag == rhs.tag && lhs.children == rhs.children
    }

    public var definition: Node? {
        switch self.tag {
        case .named(let node), .parameter(let node): node
        default: nil
        }
    }
}

extension ConstructedType {
    static func named(_ definition: Node, parameters: [Type]) -> Self {
        ConstructedType(.named(definition), children: parameters) { db, children, root in
            let wrap = !root && !children.isEmpty

            let typeName = db[definition, Defined.self]!.definition.name!

            var result = typeName
            for child in children { result += " \(child(false))" }

            return wrap ? "(\(result))" : result
        }
    }

    static func function(inputs: [Type], output: Type) -> Self {
        ConstructedType(.function, children: [output] + inputs) { _, children, root in
            let output = children[0]
            let inputs = children.dropFirst()

            var result = ""
            for input in inputs { result += "\(input(false)) " }

            result += "-> \(output(true))"

            return root ? result : "(\(result))"
        }
    }

    static func tuple(elements: [Type]) -> Self {
        ConstructedType(.tuple, children: elements) { _, children, root in
            switch children.count {
            case 0: return "()"
            case 1: return "(\(children[0](false));)"
            default:
                var result = ""
                for (index, child) in children.enumerated() {
                    if index > 0 { result += "; " }

                    result += child(true)
                }

                return "(\(result))"
            }
        }
    }

    static var unit: Self { .tuple(elements: []) }

    static func block(output: Type) -> Self {
        ConstructedType(.block, children: [output]) { _, children, root in
            let output = children[0]
            return "{\(output(true))}"
        }
    }

    static func parameter(definition: Node) -> Self {
        ConstructedType(.parameter(definition), children: []) { db, _, _ in
            db[definition, Defined.self]!.definition.name!
        }
    }
}
