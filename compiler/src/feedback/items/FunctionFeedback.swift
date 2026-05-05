import OrderedCollections

func registerFunctionFeedback(into context: FeedbackContext) {
    struct MissingInputsData {
        let function: Node
        let nodes: [Node]
        let inputs: [ConstructedType]
    }

    context.register(
        id: "missing-inputs",
        query: { db, node, body in
            Queries.conflictingTypes(db, node) { data in
                let nodes = [data.from] + data.nodes

                guard let function = withCall(db: db, nodes: nodes, as: \.function),
                    data.types.count == 2
                else { return }

                let actual = data.types[0]
                let expected = data.types[1]

                guard case .function = actual.tag, case .function = expected.tag,
                    actual.children.count < expected.children.count
                else { return }

                body(
                    MissingInputsData(
                        function: function,
                        nodes: nodes,
                        inputs: Array(expected.children.dropFirst(actual.children.count))
                            .compactMap {
                                guard case .constructed(let type) = $0 else { return nil }
                                return type
                            },
                    )
                )
            }
        },
        rank: { _ in .conflicts },
        location: { data in
            (
                primary: data.function,
                secondary: OrderedSet(data.nodes.filter { $0 != data.function }),
            )
        },
    ) { writer, data in
        writer.write(data.function)
        writer.write(" is missing ")
        writer.writeList(separator: "and", limit: 0) { list in
            for input in data.inputs {
                list.add {
                    writer.write("a ")
                    writer.write(.constructed(input))
                }
            }
        }
        writer.write(".")
        writer.writeBreak()
        writer.write("Try adding ")
        writer.write(data.inputs.count == 1 ? "this input" : "these inputs")
        writer.write(", or double-check your parentheses.")
    }

    struct ExtraInputsData {
        let function: Node
        let nodes: [Node]
        let actual: Int
        let expected: Int
    }

    context.register(
        id: "extra-input",
        query: { db, node, body in
            Queries.conflictingTypes(db, node) { data in
                let nodes = [data.from] + data.nodes

                guard let function = withCall(db: db, nodes: nodes, as: \.function),
                    data.types.count == 2
                else { return }

                let actual = data.types[0]
                let expected = data.types[1]

                guard case .function = actual.tag, case .function = expected.tag,
                    actual.children.count > expected.children.count
                else { return }

                body(
                    ExtraInputsData(
                        function: function,
                        nodes: nodes,
                        actual: actual.children.count - 1,
                        expected: expected.children.count - 1,
                    )
                )
            }
        },
        rank: { _ in .conflicts },
        location: { data in
            (
                primary: data.function,
                secondary: OrderedSet(data.nodes.filter { $0 != data.function }),
            )
        },
    ) { writer, data in
        writer.write(data.function)
        writer.write(" only needs ")
        writer.write(data.expected, singular: "input", plural: "inputs")
        writer.write(".")
        writer.writeBreak()
        writer.write("Try removing ")

        let extra = data.actual - data.expected
        if extra == 1 {
            writer.write("the extra input")
        } else {
            writer.write("\(extra) extra inputs")
        }

        writer.write(" here.")
    }

    struct NotAFunctionData {
        let function: Node
        let nodes: [Node]
    }

    context.register(
        id: "not-a-function",
        query: { db, node, body in
            Queries.conflictingTypes(db, node) { data in
                let nodes = [data.from] + data.nodes

                guard let function = withCall(db: db, nodes: nodes, as: \.function),
                    data.types.count == 2
                else { return }

                let actualIsFunction: Bool
                switch data.types[0].tag {
                case .function: actualIsFunction = true
                default: actualIsFunction = false
                }

                let expectedIsFunction: Bool
                switch data.types[1].tag {
                case .function: expectedIsFunction = true
                default: expectedIsFunction = false
                }

                guard actualIsFunction != expectedIsFunction else { return }

                body(NotAFunctionData(function: function, nodes: nodes))
            }
        },
        rank: { _ in .conflicts },
        location: { data in
            (
                primary: data.function,
                secondary: OrderedSet(data.nodes.filter { $0 != data.function }),
            )
        },
    ) { writer, data in
        writer.write(data.function)
        writer.write(" is not a function.")
        writer.writeBreak()
        writer.write("Double-check your parentheses.")
    }
}

private func withCall<T>(db: DB, nodes: some Sequence<Node>, as body: (ResolvedCall) -> T) -> T? {
    for node in nodes {
        if let parent = db[node, Parent.self]?.parent, let call = db[parent, ResolvedCall.self] {
            return body(call)
        }
    }

    return nil
}
