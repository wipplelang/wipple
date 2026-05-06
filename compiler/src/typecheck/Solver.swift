import OrderedCollections

public struct GroupedWith: Fact { public var nodes: OrderedSet<Node> = [] }

class Solver {
    static let iterationLimit = 32

    var db: DB
    var groups = Groups()
    var impliedInstances: [Instance] = []
    var substitutionsToApply: [Substitutions] = []
    var progress = false
    var error = false
    var constraints = Constraints()
    private var iterations = 0

    init(db: DB) { self.db = db }

    func sortedGroups<Key: Comparable>(by key: (Node) -> Key) -> some Sequence<Group> {
        self.applyAll()
        return self.groups.sorted(by: key)
    }

    func run() {
        while true {
            self.progress = false

            self.runPass(until: nil)

            guard self.progress else { break }
        }

        // Run a final pass
        self.runPass(until: nil)

        // Apply recorded substitutions
        for substitutions in self.substitutionsToApply { self.apply(substitutions) }
        self.substitutionsToApply = []
    }

    func runPass(until stop: Constraint.Type?) {
        guard self.iterations < Self.iterationLimit else { return }

        self.constraints.run(with: self, until: stop)

        if !self.progress { self.constraints.runDefaults(with: self) }

        self.iterations += 1
    }

    func imply(_ instance: Instance) {
        guard !self.impliedInstances.lazy.map(\.node).contains(instance.node) else { return }

        self.impliedInstances.append(instance)
    }

    func unify(_ left: Node, _ right: Node, trace: Constraint?) {
        self.unify(.node(left), .node(right), trace: trace)
    }

    func unify(_ node: Node, _ type: ConstructedType, trace: Constraint?) {
        self.unify(.node(node), .constructed(type), trace: trace)
    }

    private func unify(_ left: Type, _ right: Type, trace: Constraint?) {
        if left == right { return }

        let originalLeftNode = left.node
        let originalRightNode = right.node

        if let originalLeftNode, let originalRightNode {
            self.progress = true
            self.merge(originalLeftNode, originalRightNode, trace: trace)
            return
        }

        let left = self.applyShallow(left)
        let right = self.applyShallow(right)

        switch (left, right) {
        case (.node(let left), .node(let right)):
            self.merge(left, right, trace: trace)
            self.progress = true
        case (.node(let node), .constructed(let type)), (.constructed(let type), .node(let node)):
            self.insert(node, type, trace: trace)
            self.progress = true
        case (.constructed(let left), .constructed(let right)):
            let originalNodes = OrderedSet(
                [originalLeftNode, originalRightNode].compactMap(\.self)
            )

            if !self.unify(left, right, originalNodes: originalNodes) {
                // Report conflicts on the original nodes
                if let originalLeftNode { self.insert(originalLeftNode, right, trace: trace) }
                if let originalRightNode { self.insert(originalRightNode, left, trace: trace) }
            }
        }
    }

    private func unify(
        _ left: ConstructedType,
        _ right: ConstructedType,
        originalNodes: OrderedSet<Node>,
    ) -> Bool {
        if left.tag == right.tag {
            for (leftChild, rightChild) in zip(left.children, right.children) {
                if !leftChild.referencedNodes.intersection(originalNodes).isEmpty
                    || !rightChild.referencedNodes.intersection(originalNodes).isEmpty
                {
                    // recursive types
                    continue
                }

                self.unify(leftChild, rightChild, trace: nil)
            }
        }

        if left.tag != right.tag || left.children.count != right.children.count {
            self.error = true
            return false
        }

        return true
    }

    func unify(_ left: Substitutions, _ right: Substitutions, trace: Constraint?) {
        for (parameter, left) in left {
            if let right = right[parameter] { self.unify(left, right, trace: trace) }
        }
    }

    private func merge(_ leftNode: Node, _ rightNode: Node, trace: Constraint?) {
        self.db[leftNode, GroupedWith.self, default: .init()].nodes.append(rightNode)
        self.db[rightNode, GroupedWith.self, default: .init()].nodes.append(leftNode)

        let leftIndex = self.groups.index(of: leftNode)
        let rightIndex = self.groups.index(of: rightNode)

        if (leftIndex != nil || rightIndex != nil) && leftIndex == rightIndex {
            return  // already the same group
        }

        let (index, group): (Int?, Group) =
            switch (leftIndex, rightIndex) {
            case (let leftIndex?, let rightIndex?):
                (leftIndex, self.groups.removeExisting(at: rightIndex))
            case (let leftIndex?, nil): (leftIndex, .init(nodes: [rightNode]))
            case (nil, let rightIndex?): (rightIndex, .init(nodes: [leftNode]))
            case (nil, nil): (nil, .init(nodes: [leftNode, rightNode]))
            }

        if let index {
            var newGroup = self.groups.removeExisting(at: index)

            self.groups.merge(group, into: &newGroup, trace: trace) { left, right in
                self.unify(left, right, originalNodes: [leftNode, rightNode])
            }

            _ = self.groups.insert(newGroup)
        } else {
            _ = self.groups.insert(group)
        }
    }

    private func insert(_ node: Node, _ type: ConstructedType, trace: Constraint?) {
        var type = type
        if type.representative == nil { type.representative = node }

        let insertInto = { (group: inout Group) in
            group.types.append(type)
            if let trace { group.trace.append(trace) }
        }

        if let index = self.groups.index(of: node) {
            insertInto(&self.groups[index])
        } else {
            var group = Group(nodes: [node])
            insertInto(&group)
            _ = self.groups.insert(group)
        }
    }

    func apply(_ type: Type) -> Type { type.traverse(with: self.applyShallow(_:)) }

    func apply(_ substitutions: Substitutions) {
        for (key, type) in substitutions { substitutions[key] = self.apply(type) }
    }

    private func applyShallow(_ type: Type) -> Type {
        guard case .node(let node) = type, let index = self.groups.index(of: node) else {
            return type
        }

        let group = self.groups[index]

        guard let type = group.types.first else { return type }

        return .constructed(type)
    }

    private func applyAll() {
        for index in self.groups.indices {
            var group = self.groups.removeExisting(at: index)

            for (index, type) in group.types.enumerated() {
                group.types[index] =
                    switch self.apply(.constructed(type)) {
                    case .constructed(let type): type
                    case .node(_): fatalError("constructed types remain constructed after `apply`")
                    }
            }

            self.groups.insertExisting(group, at: index)
        }
    }
}
