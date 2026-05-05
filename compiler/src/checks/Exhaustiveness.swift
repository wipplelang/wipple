import OrderedCollections

enum MatchPathSegment: Equatable {
    case match
    case noMatch
    case field(type: Node)
    case tupleElement(index: Int, count: Int)
    case variant(type: Node)
    case variantElement(type: Node, index: Int, count: Int)
}

typealias MatchPath = [MatchPathSegment]

indirect enum MatchTree {
    case wildcard
    case field(Node, MatchTree)
    case tuple([MatchTree])
    case variant(Node, [MatchTree])

    init(path: MatchPath) {
        self = MatchTree.wildcard

        for segment in path.reversed() {
            switch segment {
            case .match, .noMatch: break
            case .field(let field): self = .field(field, self)
            case .tupleElement(let index, let count):
                var elements = Array(repeating: MatchTree.wildcard, count: count)
                elements[index] = self
                self = .tuple(elements)
            case .variant(let variant): self = .variant(variant, [])
            case .variantElement(let variant, let index, let count):
                var elements = Array(repeating: MatchTree.wildcard, count: count)
                elements[index] = self
                self = .variant(variant, elements)
            }
        }
    }

    mutating func merge(_ other: Self) {
        switch (self, other) {
        case (.wildcard, let other): self = other
        case (_, .wildcard): break
        case (.field(let leftField, let leftValue), .field(let rightField, let rightValue))
        where leftField == rightField:
            var leftValue = leftValue
            leftValue.merge(rightValue)
            self = .field(leftField, leftValue)
        case (.tuple(let leftElements), .tuple(let rightElements))
        where leftElements.count == rightElements.count:
            self = .tuple(
                zip(leftElements, rightElements)
                    .map { left, right in
                        var left = left
                        left.merge(right)
                        return left
                    }
            )
        case (
            .variant(let leftVariant, let leftElements),
            .variant(let rightVariant, let rightElements),
        ) where leftVariant == rightVariant && leftElements.count == rightElements.count:
            self = .variant(
                leftVariant,
                zip(leftElements, rightElements)
                    .map { left, right in
                        var left = left
                        left.merge(right)
                        return left
                    },
            )
        default: break
        }
    }

    func display(db: DB, root: Bool = true) -> String {
        switch self {
        case .wildcard: return "_"
        case .field(let field, let inner):
            let fieldName = db[field, Defined.self]?.definition.name ?? "_"
            return "{ \(fieldName): \(inner.display(db: db, root: false)) }"
        case .tuple(let elements):
            return "(\(elements.map { $0.display(db: db, root: false) }.joined(separator: "; ")))"
        case .variant(let variant, let elements):
            let variantName = db[variant, Defined.self]?.definition.name ?? "_"
            guard !elements.isEmpty else { return variantName }

            let rendered = elements.map { $0.display(db: db, root: false) }.joined(separator: " ")
            let result = "\(variantName) \(rendered)"
            return root ? result : "(\(result))"
        }
    }
}

extension MatchTree: Renderable {
    func render(into context: RenderContext) {
        let description = self.display(db: context.db)

        switch self {
        case .field(let node, _), .variant(let node, _): context.link(description, node)
        default: context.string(description)
        }
    }
}

struct Matches: Fact {
    let value: Node
    let arm: Node?
    let path: MatchPath?

    func render(into context: RenderContext) {}
}

struct MissingPatterns: Fact {
    var patterns: [MatchTree]

    func render(into context: RenderContext) { context.string("is missing patterns") }
}

func checkExhaustiveness(db: DB) {
    var values: OrderedDictionary<Node, OrderedDictionary<Node, [MatchPath]>> = [:]

    db.forEachFact(Matches.self) { _, matches in
        guard let arm = matches.arm, let path = matches.path else { return }

        values[matches.value, default: [:]][arm, default: []].append(path)
    }

    for (value, groups) in values {
        let actual = groups.values.filter { !$0.isEmpty }
        let maxDepth = actual.lazy.joined().map(\.count).max() ?? 0

        let expected: [[MatchPath]] =
            if let type = db.type(of: value) {
                collectPaths(
                    db: db,
                    type: type,
                    substitutions: Substitutions(),
                    prefix: [],
                    max: maxDepth,
                )
            } else { [] }

        let missing = expected.filter { expected in
            !actual.contains { actual in matchesCoverage(actual: actual, expected: expected) }
        }

        guard !missing.isEmpty else { continue }

        db[value, MissingPatterns.self] = .init(
            patterns: missing.map { coverage in
                coverage.compactMap(MatchTree.init)
                    .reduce(into: MatchTree.wildcard) { $0.merge($1) }
            }
        )
    }
}

private func collectPaths(
    db: DB,
    type: ConstructedType,
    substitutions: Substitutions,
    prefix: MatchPath,
    max: Int,
) -> [[MatchPath]] {
    guard prefix.count < max else { return [[prefix]] }

    var type = type
    if case .parameter(let parameter) = type.tag, let substitution = substitutions[parameter],
        case .constructed(let constructed) = substitution
    {
        type = constructed
    }

    switch type.tag {
    case .named(let definitionNode):
        guard let definition = db[definitionNode, Defined.self]?.definition as? TypeDefinition
        else { return [] }

        for (parameter, type) in zip(definition.parameters, type.children) {
            substitutions[parameter] = type
        }

        if definition.attributes.intrinsic {
            var prefix = prefix
            prefix.append(.noMatch)
            return [[prefix]]
        } else if let fields = db[definitionNode, StructureFields.self]?.fields {
            return cartesianProduct(
                fields.lazy.map { field -> [[MatchPath]] in
                    guard let fieldType = db.type(of: field) else { return [] }

                    var prefix = prefix
                    prefix.append(.field(type: field))

                    return collectPaths(
                        db: db,
                        type: fieldType,
                        substitutions: substitutions,
                        prefix: prefix,
                        max: max,
                    )
                }
            )
            .map { combination in Array(combination.joined()) }
        } else if let variants = db[definitionNode, EnumerationVariants.self]?.variants {
            return variants.flatMap { variant, elements -> [[MatchPath]] in
                if elements.isEmpty {
                    var prefix = prefix
                    prefix.append(.variant(type: variant))
                    return [[prefix]]
                }

                return cartesianProduct(
                    elements.enumerated()
                        .map { index, element in
                            guard let elementType = db.type(of: element) else { return [] }

                            var prefix = prefix
                            prefix.append(
                                .variantElement(type: variant, index: index, count: elements.count)
                            )

                            return collectPaths(
                                db: db,
                                type: elementType,
                                substitutions: substitutions,
                                prefix: prefix,
                                max: max,
                            )
                        }
                )
                .map { combination in Array(combination.joined()) }
            }
        } else {
            return []
        }
    case .tuple:
        return cartesianProduct(
            type.children.enumerated()
                .map { index, element in
                    guard case .constructed(let elementType) = element else { return [] }

                    var prefix = prefix
                    prefix.append(.tupleElement(index: index, count: type.children.count))

                    return collectPaths(
                        db: db,
                        type: elementType,
                        substitutions: substitutions,
                        prefix: prefix,
                        max: max,
                    )
                }
        )
        .map { combination in Array(combination.joined()) }
    default:
        var prefix = prefix
        prefix.append(.match)
        return [[prefix]]
    }
}

private func matchesCoverage(actual: [MatchPath], expected: [MatchPath]) -> Bool {
    expected.allSatisfy { expected in
        actual.contains { actual in
            for (segment, other) in zip(actual, expected) {
                switch (segment, other) {
                case (.match, _): return true  // short-circuit
                case (.field(let field), .field(let otherField)) where field != otherField:
                    return true  // allow omitted fields
                case (.field(let field), .field(let otherField)) where field == otherField: break
                case (
                    .tupleElement(let index, let count),
                    .tupleElement(let otherIndex, let otherCount),
                ) where index == otherIndex && count == otherCount: break
                case (.variant(let type), .variant(let otherType)) where type == otherType: break
                case (
                    .variantElement(let type, let index, let count),
                    .variantElement(let otherType, let otherIndex, let otherCount),
                ) where type == otherType && index == otherIndex && count == otherCount: break
                default: return false
                }
            }

            return true
        }
    }
}

private func cartesianProduct(_ groups: some Sequence<[[MatchPath]]>) -> [[[MatchPath]]] {
    groups.reduce([[]]) { result, group in
        result.flatMap { partial in group.map { partial + [$0] } }
    }
}
