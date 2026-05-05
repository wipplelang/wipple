public struct FormatExpressionSyntax: Sendable {
    public let span: Span
    public let string: Substring
    public let inputs: [any Visitable]
}

public func parseFormatExpression(with parser: Parser) throws(ParseError) -> FormatExpressionSyntax
{
    let span = parser.spanned()
    let string = try parser.token(.string)
    let inputs = try parser.parseMany(min: 1, parseAtomicExpression)
    return FormatExpressionSyntax(span: span(), string: string, inputs: inputs)
}

struct MissingFormatInputs: Fact {
    let count: Int

    func render(into context: RenderContext) {
        context.string("is missing \(self.count) format inputs")
    }
}

struct ExtraFormatInput: Fact {
    func render(into context: RenderContext) { context.string("is extra format input") }
}

extension FormatExpressionSyntax: Visitable {
    public func visit(node: Node, with visitor: Visitor) {
        visitExpression(node: node, with: visitor)

        var segments = self.string.split(separator: "_", omittingEmptySubsequences: false)
        let trailing = segments.popLast() ?? ""

        let stringType = visitor.visit(
            NamedTypeSyntax(span: self.span, name: "String", parameters: [])
        )

        visitor.constraint(GroupConstraint(node, stringType))

        let inputs = self.inputs.map { visitor.visit($0) }

        var formatSegments: [FormatExpressionCodegen.Segment] = []
        let (missing, extra) = exactForEach(segments, inputs) { segment, input in
            visitor.db.edge(from: input, to: node, label: "input")

            let describeNode = visitor.visit(
                ConstructorExpressionSyntax(span: self.span, constructor: "Describe")
            )

            visitor.constraint(
                TypeConstraint(
                    describeNode,
                    .function(inputs: [.node(input)], output: .node(stringType)),
                )
            )

            let temporary = visitor.db.node()
            visitor.constraint(GroupConstraint(temporary, stringType))

            formatSegments.append(
                .init(
                    string: String(segment),
                    describeNode: describeNode,
                    input: input,
                    temporary: temporary,
                )
            )
        }

        if !missing.isEmpty {
            visitor.db[node, MissingFormatInputs.self] = .init(count: missing.count)
        }

        for node in extra { visitor.db[node, ExtraFormatInput.self] = .init() }

        visitor.codegen(
            node: node,
            with: FormatExpressionCodegen(
                node: node,
                segments: formatSegments,
                trailing: String(trailing),
            ),
        )
    }
}

private struct FormatExpressionCodegen: Codegenable {
    struct Segment {
        let string: String
        let describeNode: Node
        let input: Node
        let temporary: Node
    }

    let node: Node
    let segments: [Segment]
    let trailing: String

    func codegen(with context: CodegenContext) throws {
        let segments = try self.segments.map { segment in
            try context.codegen(segment.describeNode)
            try context.codegen(segment.input)

            context.instruction(
                .value(
                    node: segment.temporary,
                    value: .call(function: segment.describeNode, inputs: [segment.input]),
                )
            )

            return (segment.string, segment.temporary)
        }

        context.instruction(
            .value(node: self.node, value: .concat(segments: segments, trailing: self.trailing))
        )
    }
}
