import OrderedCollections

public struct CodegenError: Error, CustomStringConvertible {
    public let description: String

    public init(_ description: String) { self.description = description }
}

protocol Codegenable { func codegen(with context: CodegenContext) throws }

struct Codegen: Fact {
    let value: any Codegenable

    func render(into context: RenderContext) {}
}

class CodegenContext {
    let db: DB
    let monomorphizeContext: MonomorphizeContext
    var instructions: [[IR.Instruction]] = [[]]
    var conditions: [[IR.Condition]] = []

    init(db: DB) {
        self.db = db
        self.monomorphizeContext = .init(db: db)
    }

    func codegen(_ node: Node) throws {
        guard let codegen = self.db[node, Codegen.self]?.value else { return }

        try codegen.codegen(with: self)
    }

    func pushInstructions() { self.instructions.append([]) }

    func popInstructions() -> [IR.Instruction] { self.instructions.removeLast() }

    func instruction(_ instruction: IR.Instruction) {
        self.instructions[self.instructions.count - 1].append(instruction)
    }

    func pushConditions() { self.conditions.append([]) }

    func popConditions() -> [IR.Condition] { self.conditions.removeLast() }

    func condition(_ condition: IR.Condition) {
        self.conditions[self.conditions.count - 1].append(condition)
    }

    func definitionKey(
        for definition: Node,
        substitutions: Substitutions,
        bounds: OrderedDictionary<Node, IR.Instance>,
        isGeneric: Bool,
    ) throws -> IR.DefinitionKey {
        try self.monomorphizeContext.getOrInsert(
            definition: definition,
            substitutions: substitutions,
            bounds: bounds,
            isGeneric: isGeneric,
        )
    }

    func codegenConstant(
        definition: Node,
        substitutions: Substitutions,
        bounds: Bounds,
        isGeneric: Bool,
    ) throws -> IR.DefinitionKey {
        let bounds = try bounds.bounds.mapValues { _, resolved in
            guard let resolved else { throw CodegenError("unresolved") }
            return try self.codegenInstance(resolved, isGeneric: true)
        }

        return try self.definitionKey(
            for: definition,
            substitutions: substitutions,
            bounds: bounds,
            isGeneric: isGeneric,
        )
    }

    func codegenInstance(_ resolved: ResolvedBound, isGeneric: Bool) throws -> IR.Instance {
        if resolved.instance.isFromBound { return .bound(resolved.instance.node) }

        let bounds = self.db[resolved.resolvedNode, Bounds.self] ?? .init()
        return .definition(
            try self.codegenConstant(
                definition: resolved.instance.node,
                substitutions: resolved.instanceSubstitutions,
                bounds: bounds,
                isGeneric: isGeneric,
            )
        )
    }
}

public func codegen(db: DB, files: [Node], libraryFiles: [Node]) throws -> IR.Program {
    let context = CodegenContext(db: db)

    var program = IR.Program()

    for file in [files, libraryFiles].joined() {
        guard let span = db[file, Syntax.self]?.value.span else {
            throw CodegenError("not a file: \(file)")
        }

        program.files.append(span)
        try context.codegen(file)
    }

    program.topLevel.instructions = context.popInstructions()

    for instruction in program.topLevel.instructions {
        try instruction.forEachNode { node in
            program.topLevel.types[node] = try db.irType(for: .node(node))
        }
    }

    program.definitions.merge(
        try context.monomorphizeContext.monomorphizeDefinitions(),
        uniquingKeysWith: { $1 },
    )

    return program
}
