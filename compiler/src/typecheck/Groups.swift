import HashTreeCollections

public struct Group {
    public var nodes: [Node]
    public var types: [ConstructedType] = []
    public var trace: [Constraint] = []

    mutating func unify(
        with types: some Sequence<ConstructedType>,
        representative: Node?,
        using unify: (ConstructedType, ConstructedType) -> Bool,
    ) {
        for type in types {
            if self.types.isEmpty || !unify(self.types[0], type) {
                // If the type cannot be unified, add it to the group separately

                guard !self.types.contains(type) else { continue }

                var type = type
                if let representative, type.representative == nil {
                    type.representative = representative
                }

                self.types.append(type)
            }
        }
    }
}

struct Groups {
    private enum Slot {
        case group(Group)
        case empty

        var group: Group? {
            switch self {
            case .group(let group): return group
            case .empty: return nil
            }
        }
    }

    private var slots: TreeDictionary<Int, Slot> = [:]

    func index(of node: Node) -> Int? {
        self.slots.first { _, slot in
            guard let group = slot.group else { return false }
            return group.nodes.contains(node)
        }?
        .key
    }

    subscript(index: Int) -> Group {
        get { self.slots[index]!.group! }
        set { self.slots[index] = .group(newValue) }
    }

    mutating func insert(_ group: Group) -> Int {
        if let index = self.slots.first(where: { _, slot in slot.group == nil })?.key {
            self.slots[index] = .group(group)
            return index
        }

        let index = self.slots.count
        self.slots[index] = .group(group)
        return index
    }

    mutating func insertExisting(_ group: Group, at index: Int) {
        self.slots[index] = .group(group)
    }

    mutating func removeExisting(at index: Int) -> Group {
        let group = self.slots[index]!.group!
        self.slots[index] = .empty
        return group
    }

    func merge(
        _ oldGroup: Group,
        into newGroup: inout Group,
        trace: Constraint?,
        using unify: (ConstructedType, ConstructedType) -> Bool,
    ) {
        newGroup.nodes.append(contentsOf: oldGroup.nodes)
        newGroup.unify(with: oldGroup.types, representative: nil, using: unify)
        newGroup.trace.append(contentsOf: oldGroup.trace)

        if let trace { newGroup.trace.append(trace) }
    }

    var indices: some Sequence<Int> {
        self.slots.compactMap { key, slot in slot.group != nil ? key : nil }
    }

    func sorted<Key: Comparable>(by key: (Node) -> Key) -> some Sequence<Group> {
        self.slots.values.compactMap(\.group)
            .map { group in
                var group = group
                group.nodes.sort { key($0) < key($1) }
                return group
            }
    }
}

public class Typed: Fact {
    public var group: Group?

    public func render(into context: RenderContext) {
        guard let group = self.group else {
            context.string("types not solved")
            return
        }

        if group.types.isEmpty {
            context.string("missing type")

            if group.nodes.count > 1 {
                context.string(" (group: ")

                for (index, node) in group.nodes.enumerated() {
                    if index > 0 { context.string(", ") }
                    context.node(node)
                }

                context.string(")")
            }

            return
        }

        context.string("has type ")

        for (index, type) in group.types.enumerated() {
            if index > 0 { context.string(" or ") }

            Type.constructed(type).render(into: context)
        }
    }
}

extension DB {
    public func type(of node: Node) -> ConstructedType? {
        guard let typed = self[node, Typed.self], let group = typed.group, group.types.count == 1
        else { return nil }

        return group.types[0]
    }
}
