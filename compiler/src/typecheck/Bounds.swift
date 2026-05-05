import OrderedCollections

public struct Instance {
    public var node: Node
    public var traitNode: Node
    public var substitutions: Substitutions
    public var isFromBound = false
    public var isDefault = false
    public var isError = false
}

public struct Instances: Fact {
    public var instances: [Instance] = []

    public func render(into context: RenderContext) { context.string("has instances") }
}

public struct Bounds: Fact {
    public var bounds: OrderedDictionary<Node, (Bound, ResolvedBound?)> = [:]

    public func render(into context: RenderContext) {
        guard !self.bounds.isEmpty else {
            context.string("has bound(s)")
            return
        }

        context.string("has bound(s) ")

        for (index, (_, (bound, instance))) in self.bounds.enumerated() {
            if index > 0 { context.string(", ") }

            bound.render(into: context)

            context.string(" (")
            if let resolved = instance {
                context.node(resolved.instance.node)
            } else {
                context.string("unresolved")
            }
            context.string(")")
        }
    }
}

public struct ResolvedBound {
    public let instance: Instance
    public let instanceSubstitutions: Substitutions
    public let resolvedNode: Node
    public let resolvedSubstitutions: Substitutions
}

public struct Bound {
    public var sourceNode: Node
    public var boundNode: Node
    public var traitNode: Node
    public var targetNode: Node?
    public var substitutions: Substitutions
    public var isOptional = false
}

extension Instance {
    public static func resolved(_ node: Node, from bound: Bound) -> Self {
        return .init(
            node: node,
            traitNode: bound.traitNode,
            substitutions: bound.substitutions,
            isFromBound: true,
        )
    }
}

extension Bound: Renderable {
    public func render(into context: RenderContext) {
        let traitDefinition =
            context.db[self.traitNode, Defined.self]!.definition as! TraitDefinition

        context.code(traitDefinition.name!)

        for parameter in traitDefinition.parameters {
            context.string(" ")
            if let type = self.substitutions[parameter] {
                type.render(into: context, root: false)
            } else {
                context.code("_")
            }
        }
    }
}
