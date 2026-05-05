import Foundation
import OrderedCollections

extension IR.Program {
    public func wasm(db: DB, options: WasmBackend.Options = .init()) throws -> String {
        let backend = WasmBackend(db: db, options: options)
        try backend.write(self)
        return backend.finish()
    }
}

public final class WasmBackend {
    public struct Options {
        public let trace: Set<String>

        public init(trace: Set<String> = []) { self.trace = trace }
    }

    private struct Function {
        let value: IR.Value
        let types: OrderedDictionary<Node, IR.`Type`>
    }

    private struct RuntimeFunction: Hashable {
        enum ValueType: Hashable {
            case type(IR.`Type`)
            case string(String)
        }

        let mangled: String
        let inputs: [ValueType]
        let outputs: [ValueType]
    }

    let db: DB
    let options: Options

    private var output = ""
    private var types: OrderedSet<IR.`Type`> = []
    private var functions: OrderedDictionary<IR.DefinitionKey?, OrderedDictionary<Node, Function>> =
        [:]
    private var strings: [(Int, String)] = []
    private var runtimeFunctions: OrderedDictionary<String, OrderedSet<RuntimeFunction>> = [:]

    init(db: DB, options: Options) {
        self.db = db
        self.options = options
    }

    func write(_ program: IR.Program) throws {
        var imports = ""
        var declarations = ""
        var functions = ""

        imports += "(module\n"
        declarations += "(rec\n"

        functions += "(func $main (export \"main\")\n"
        try self.writeDefinition(into: &functions, key: nil, definition: program.topLevel)
        functions += "(return)\n"
        functions += ")\n"

        for (key, definition) in program.definitions {
            guard let type = definition.type else {
                throw CodegenError("unresolved definition type: \(key)")
            }

            functions += "(func $\(key.mangled) (result \(self.structuralType(type)))\n"
            try self.writeDefinition(into: &functions, key: key, definition: definition)
            functions += ")\n"
        }

        while !self.functions.isEmpty {
            let queuedFunctions = self.functions
            self.functions.removeAll(keepingCapacity: true)

            for (definition, queuedFunctions) in queuedFunctions {
                for (node, queuedFunction) in queuedFunctions {
                    try self.writeFunction(
                        imports: &imports,
                        declarations: &declarations,
                        functions: &functions,
                        node: node,
                        value: queuedFunction.value,
                        definition: definition,
                        types: queuedFunction.types,
                    )
                }
            }
        }

        declarations += "(type $number (struct (field f64)))\n"
        declarations += "(type $box (struct (field (mut anyref))))\n"

        for type in self.types { try self.writeType(into: &declarations, type: type) }
        declarations += ")\n"

        self.writeMemory(into: &declarations)

        for (name, runtimeFunctions) in self.runtimeFunctions {
            for runtimeFunction in runtimeFunctions {
                imports += "(func $\(runtimeFunction.mangled) (import \"runtime\" \"\(name)\")"

                for input in runtimeFunction.inputs {
                    imports += " (param \(self.runtimeType(input)))"
                }

                for output in runtimeFunction.outputs {
                    imports += " (result \(self.runtimeType(output)))"
                }

                imports += ")\n"
            }
        }

        self.output = imports + declarations + functions + ")"
    }

    func writeDefinition(
        into output: inout String,
        key: IR.DefinitionKey?,
        definition: IR.Definition,
    ) throws {
        try self.writeLocals(
            into: &output,
            instructions: definition.instructions,
            types: definition.types,
            shouldDeclare: { _ in true },
        )

        try self.writeInstructions(
            into: &output,
            instructions: definition.instructions,
            key: key,
            types: definition.types,
        )
    }

    private func writeLocals(
        into output: inout String,
        instructions: [IR.Instruction],
        types: OrderedDictionary<Node, IR.`Type`>,
        shouldDeclare: (Node) -> Bool,
    ) throws {
        var nodes: OrderedSet<Node> = []

        for instruction in instructions {
            instruction.forEachNode(traverseFunctions: false) { node in nodes.append(node) }
        }

        for node in nodes {
            guard shouldDeclare(node) else { continue }

            output += "(local $\(node.mangled)"

            if self.db.contains(IsMutated.self, for: node) {
                output += " (ref null $box)"
            } else {
                let type = try self.type(of: node, in: types)
                output += " \(self.structuralType(type))"
            }

            output += ")\n"
        }
    }

    private func writeInstructions(
        into output: inout String,
        instructions: [IR.Instruction],
        key: IR.DefinitionKey?,
        types: OrderedDictionary<Node, IR.`Type`>,
    ) throws {
        for instruction in instructions {
            switch instruction {
            case .if(let node, let branches, let elseBranch):
                for branch in branches {
                    output += "(if "
                    try self.writeConditions(
                        into: &output,
                        conditions: branch.conditions,
                        key: key,
                        types: types,
                    )
                    output += " (then\n"
                    try self.writeInstructions(
                        into: &output,
                        instructions: branch.instructions,
                        key: key,
                        types: types,
                    )

                    if let node, let result = branch.result {
                        output += "(local.set $\(node.mangled) (local.get $\(result.mangled)))\n"
                    }

                    output += ") (else"
                }

                if let elseBranch {
                    try self.writeInstructions(
                        into: &output,
                        instructions: elseBranch.instructions,
                        key: key,
                        types: types,
                    )

                    if let node, let result = elseBranch.result {
                        output += "(local.set $\(node.mangled) (local.get $\(result.mangled)))\n"
                    }
                } else {
                    output += "(unreachable)"
                }

                for _ in branches { output += "))\n" }
            case .return(let value): output += "(return (local.get $\(value.mangled)))\n"
            case .value(let node, let value):
                output += "(local.set $\(node.mangled) "
                try self.writeValue(
                    into: &output,
                    node: node,
                    value: value,
                    key: key,
                    types: types,
                )
                output += ")\n"
            case .trace(let location):
                guard self.canTrace(location: location) else { continue }

                let name = try self.runtime(
                    name: "trace",
                    inputs: [.string("externref")],
                    outputs: [],
                )

                let span = self.db[location, Syntax.self]?.value.span ?? .empty

                let string = try self.string(
                    String(data: try! JSONEncoder().encode(span.start), encoding: .utf8)!
                )

                output += "(call $\(name) \(string))\n"
            }
        }
    }

    private func writeValue(
        into output: inout String,
        node: Node?,
        value: IR.Value,
        key: IR.DefinitionKey?,
        types: OrderedDictionary<Node, IR.`Type`>,
    ) throws {
        switch value {
        case .bound(let node): throw CodegenError("bound \(node) not resolved")
        case .call(let function, let inputs):
            let type = try self.type(of: function, in: types)
            output += "(call_ref $impl_\(type.nominal)"
            output += "(struct.get $\(type.nominal) 1 (local.get $\(function.mangled)))"

            for input in inputs { output += "(local.get $\(input.mangled))" }

            output += "(struct.get $\(type.nominal) 0 (local.get $\(function.mangled))))"
        case .concat(let segments, let trailing):
            for (string, node) in segments {
                let name = try self.runtime(
                    name: "concat",
                    inputs: [.string("externref"), .string("externref")],
                    outputs: [.string("externref")],
                )
                let string = try self.string(string)
                output += "(call $\(name) \(string) "
                output += "(call $\(name) (local.get $\(node.mangled))"
            }

            output += try self.string(trailing)

            for _ in segments { output += "))" }
        case .constant(let key): output += "(call $\(key.mangled))"
        case .function(_, let captures, _):
            guard let node else { throw CodegenError("missing node") }

            let type = try self.type(of: node, in: types)
            self.functions[key, default: [:]][node] = .init(value: value, types: types)

            var envType = "$env"
            if let key { envType += key.mangled }
            envType += node.mangled

            output += "(struct.new $\(type.nominal)"
            output += "(ref.func $"
            if let key { output += key.mangled }
            output += "\(node.mangled))"
            output += "(struct.new \(envType)"
            for capture in captures { output += "(local.get $\(capture.mangled))" }
            output += "))"
        case .field(let input, _, let fieldIndex):
            let type = try self.type(of: input, in: types)
            output += "(struct.get $\(type.nominal) \(fieldIndex) (local.get $\(input.mangled)))"
        case .tuple(let elements):
            guard let node else { throw CodegenError("missing node") }

            let type = try self.type(of: node, in: types)
            output += "(struct.new $\(type.nominal)"
            for element in elements { output += "(local.get $\(element.mangled))" }
            output += ")"
        case .marker:
            guard let node else { throw CodegenError("missing node") }

            let type = try self.type(of: node, in: types)
            output += "(struct.new $\(type.nominal))"
        case .mutableVariable(let variable):
            guard let node else { throw CodegenError("missing node") }

            let type = try self.type(of: node, in: types)
            output += "(struct.get $box 0 (local.get $\(variable.mangled)))"

            if let from = self.conversions(for: type).from { output += from }
        case .number(let number): output += "(f64.const \(number))"
        case .runtime(let name, let inputs):
            if name.contains(".") {
                // Treat as a Wasm instruction

                output += "(\(name)"
                for input in inputs { output += " (local.get $\(input.mangled))" }
                output += ")"
            } else {
                // Treat as an external function call

                guard let node else { throw CodegenError("missing node") }

                let outputType = try self.type(of: node, in: types)
                let inputTypes = try inputs.map { try self.type(of: $0, in: types) }

                let maybeOutputType: IR.`Type`? =
                    if case .named(_, let parameters, _, _, let abi) = outputType, abi == "maybe" {
                        parameters.first
                    } else { nil }

                let runtimeName =
                    if let maybeOutputType {
                        try self.runtime(
                            name: name,
                            inputs: inputTypes.map(RuntimeFunction.ValueType.type),
                            outputs: [.type(maybeOutputType), .string("i32")],
                        )
                    } else {
                        try self.runtime(
                            name: name,
                            inputs: inputTypes.map(RuntimeFunction.ValueType.type),
                            outputs: [.type(outputType)],
                        )
                    }

                output += "(call $\(runtimeName)"
                for input in inputs { output += "(local.get $\(input.mangled))" }
                output += ")"

                if let maybeOutputType {
                    output +=
                        "(if (param \(self.structuralType(maybeOutputType))) (result \(self.structuralType(outputType))) (then "
                    output +=
                        "(struct.new $\(outputType.nominal) (struct.new $\(outputType.nominal)_variant1) (i32.const 1))"
                    output += ") (else "
                    output +=
                        "(drop) (struct.new $\(outputType.nominal) (struct.new $\(outputType.nominal)_variant0) (i32.const 0))"
                    output += "))"
                }
            }
        case .string(let string): output += try self.string(string)
        case .structure(let fields):
            guard let node else { throw CodegenError("missing node") }

            let type = try self.type(of: node, in: types)
            output += "(struct.new $\(type.nominal)"
            for (_, value) in fields { output += " (local.get $\(value.mangled))" }
            output += ")"
        case .tupleElement(let input, let index):
            let type = try self.type(of: input, in: types)
            output += "(struct.get $\(type.nominal) \(index) (local.get $\(input.mangled)))"
        case .unreachable: output += "(unreachable)"
        case .variable(let node): output += "(local.get $\(node.mangled))"
        case .variant(let index, let elements):
            guard let node else { throw CodegenError("missing node") }

            let type = try self.type(of: node, in: types)
            output += "(struct.new $\(type.nominal) "
            output += "(struct.new $\(type.nominal)_variant\(index)"
            for element in elements { output += " (local.get $\(element.mangled))" }
            output += ") (i32.const \(index)))"
        case .variantElement(let input, let variant, let index):
            let type = try self.type(of: input, in: types)
            output +=
                "(struct.get $\(type.nominal)_variant\(variant) \(index) (ref.cast (ref null $\(type.nominal)_variant\(variant)) (struct.get $\(type.nominal) 0 (local.get $\(input.mangled)))))"
        }
    }

    private func writeConditions(
        into output: inout String,
        conditions: [IR.Condition],
        key: IR.DefinitionKey?,
        types: OrderedDictionary<Node, IR.`Type`>,
    ) throws {
        if conditions.isEmpty {
            output += "(i32.const 1)"
            return
        }

        for (index, condition) in conditions.enumerated() {
            output += "(if (result i32) "
            try self.writeCondition(into: &output, condition: condition, key: key, types: types)
            output += "(then "

            if index == conditions.count - 1 { output += "(i32.const 1)" }
        }

        for _ in conditions { output += ") (else (i32.const 0)))" }
    }

    private func writeCondition(
        into output: inout String,
        condition: IR.Condition,
        key: IR.DefinitionKey?,
        types: OrderedDictionary<Node, IR.`Type`>,
    ) throws {
        switch condition {
        case .or(let branches):
            if branches.isEmpty {
                output += "(i32.const 1)"
                return
            }

            for (index, conditions) in branches.enumerated() {
                output += "(if (result i32) "
                try self.writeConditions(
                    into: &output,
                    conditions: conditions,
                    key: key,
                    types: types,
                )
                output += "(then (i32.const 1)) (else "

                if index == branches.count - 1 { output += "(i32.const 0)" }
            }

            for _ in branches { output += "))" }
        case .equalToNumber(let input, let value):
            output += "(f64.eq (local.get $\(input.mangled)) (f64.const \(value)))"
        case .equalToString(let input, let value):
            let name = try self.runtime(
                name: "string-equality",
                inputs: [.string("externref"), .string("externref")],
                outputs: [.string("i32")],
            )
            let string = try self.string(value)
            output += "(call $\(name) (local.get $\(input.mangled)) \(string))"
        case .equalToVariant(let input, let variant):
            let type = try self.type(of: input, in: types)
            output +=
                "(i32.eq (struct.get $\(type.nominal) 1 (local.get $\(input.mangled))) (i32.const \(variant)))"
        case .initialize(let variable, let node, let value, let mutable):
            output += "(block (result i32) (local.set $\(variable.mangled) "

            var to: String?
            if mutable {
                output += "(struct.new $box "

                guard let node else { throw CodegenError("missing node") }

                let valueType = try self.type(of: node, in: types)
                to = self.conversions(for: valueType).to
            }

            try self.writeValue(into: &output, node: node, value: value, key: key, types: types)

            if let to { output += to }

            if mutable { output += ")" }

            output += ") (i32.const 1))"
        case .mutate(let input, let variable):
            let type = try self.type(of: variable, in: types)
            output += "(block (result i32) (struct.set $box 0 (local.get $\(variable.mangled)) "
            output += "(local.get $\(input.mangled))"
            if let to = self.conversions(for: type).to { output += to }
            output += ") (i32.const 1))"
        }
    }

    private func writeFunction(
        imports: inout String,
        declarations: inout String,
        functions: inout String,
        node: Node,
        value: IR.Value,
        definition: IR.DefinitionKey?,
        types: OrderedDictionary<Node, IR.`Type`>,
    ) throws {
        guard case .function(let inputs, let captures, let instructions) = value else {
            fatalError("unreachable")
        }

        let type = try self.type(of: node, in: types)
        guard case .function(let inputTypes, let outputType) = type else {
            throw CodegenError("\(node) is not a function")
        }

        var envType = "$env"
        if let definition { envType += definition.mangled }
        envType += node.mangled

        declarations += "(type \(envType) (struct"
        for capture in captures {
            if self.db.contains(IsMutated.self, for: capture) {
                declarations += "(field (ref null $box))"
            } else {
                let type = try self.type(of: capture, in: types)
                declarations += "(field \(self.structuralType(type)))"
            }
        }
        declarations += "))\n"

        imports += "(elem declare func $"
        if let definition { imports += definition.mangled }
        imports += "\(node.mangled))\n"

        functions += "(func $"
        if let definition { functions += definition.mangled }
        functions += "\(node.mangled)\n"
        functions += "(type $impl_\(type.nominal)) (param $env anyref)\n"

        for (input, type) in zip(inputs, inputTypes) {
            functions += "(param $\(input.mangled) \(self.structuralType(type)))\n"
        }

        functions += "(result \(self.structuralType(outputType)))\n"

        for capture in captures {
            functions += "(local $\(capture.mangled) "
            if self.db.contains(IsMutated.self, for: capture) {
                functions += "(ref null $box)"
            } else {
                let type = try self.type(of: capture, in: types)
                functions += self.structuralType(type)
            }
            functions += ")\n"
        }

        try self.writeLocals(
            into: &functions,
            instructions: instructions,
            types: types,
            shouldDeclare: { child in !inputs.contains(child) && !captures.contains(child) },
        )

        for (index, capture) in captures.enumerated() {
            var envType = "$env"
            if let definition { envType += definition.mangled }
            envType += node.mangled

            functions +=
                "(local.set $\(capture.mangled) (struct.get \(envType) \(index) (ref.cast (ref null \(envType)) (local.get $env))))\n"
        }

        try self.writeInstructions(
            into: &functions,
            instructions: instructions,
            key: definition,
            types: types,
        )

        functions += ")\n"
    }

    private func writeType(into output: inout String, type: IR.`Type`) throws {
        switch type {
        case .named(let definition, let parameters, _, _, _):
            let representation = try self.db.irNamedTypeRepresentation(
                definition: definition,
                parameters: parameters,
            )

            switch representation {
            case .intrinsic: break
            case .marker: output += "(type $\(type.nominal) (struct))\n"
            case .structure(let fields):
                output += "(type $\(type.nominal) (struct"
                for field in fields { output += " (field \(self.structuralType(field)))" }
                output += "))\n"
            case .enumeration(let variants):
                output += "(type $\(type.nominal) (struct (field anyref) (field i32)))\n"

                for (index, elements) in variants.enumerated() {
                    output += "(type $\(type.nominal)_variant\(index) (struct"
                    for element in elements { output += " (field \(self.structuralType(element)))" }
                    output += "))\n"
                }
            }
        case .tuple(let elements):
            output += "(type $\(type.nominal) (struct"
            for element in elements { output += " (field \(self.structuralType(element)))" }
            output += "))\n"
        case .function(let inputs, let outputType):
            output +=
                "(type $\(type.nominal) (struct (field (ref null $impl_\(type.nominal))) (field anyref)))\n"
            output += "(type $impl_\(type.nominal) (func (param anyref)"
            for input in inputs { output += " (param \(self.structuralType(input)))" }
            output += " (result \(self.structuralType(outputType)))))\n"
        case .parameter(let definition): throw CodegenError("parameter \(definition) not resolved")
        }
    }

    private func canTrace(location: Node) -> Bool {
        guard let path = self.db[location, Syntax.self]?.value.span.path else { return false }

        return self.options.trace.contains(path)
    }

    private func type(of node: Node, in types: OrderedDictionary<Node, IR.`Type`>) throws
        -> IR.`Type`
    {
        guard let type = types[node] else { throw CodegenError("missing type for node \(node)") }

        _ = type.traverse { type in
            self.types.append(type)
            return type
        }

        return type
    }

    private func structuralType(_ type: IR.`Type`) -> String {
        if let structural = type.structural {
            return "(ref null $\(structural))"
        } else {
            guard case .named(_, _, let intrinsic, let representation, _) = type else {
                fatalError("unreachable")
            }

            if let representation {
                return representation
            } else if intrinsic {
                return "externref"
            } else {
                fatalError("unreachable")
            }
        }
    }

    private func conversions(for type: IR.`Type`) -> (from: String?, to: String?) {
        if case .named(_, _, let intrinsic, let representation, _) = type {
            if representation == "f64" {
                return (
                    from: "(ref.cast (ref null $number)) (struct.get $number 0)",
                    to: "(struct.new $number)",
                )
            } else if intrinsic {
                return (from: "(extern.convert_any)", to: "(any.convert_extern)")
            }
        }

        return (from: "(ref.cast \(self.structuralType(type)))", to: nil)
    }

    private func runtime(
        name: String,
        inputs: [RuntimeFunction.ValueType],
        outputs: [RuntimeFunction.ValueType],
    ) throws -> String {
        var mangled = name

        for valueType in [inputs, outputs].joined() {
            if case .type(let type) = valueType { mangled += "_\(type.nominal)" }
        }

        self.runtimeFunctions[name, default: []]
            .append(.init(mangled: mangled, inputs: inputs, outputs: outputs))

        return mangled
    }

    private func runtimeType(_ type: RuntimeFunction.ValueType) -> String {
        switch type {
        case .type(let type): return self.structuralType(type)
        case .string(let string): return string
        }
    }

    private func string(_ string: String) throws -> String {
        let offset = {
            if let (offset, _) = self.strings.first(where: { _, s in s == string }) {
                return offset
            } else {
                let offset =
                    if let (offset, s) = self.strings.last { offset + s.utf16.count } else { 0 }

                self.strings.append((offset, string))

                return offset
            }
        }()

        let name = try self.runtime(
            name: "make-string",
            inputs: [.string("i32"), .string("i32")],
            outputs: [.string("externref")],
        )

        return "(call $\(name) (i32.const \(offset)) (i32.const \(string.utf8.count)))"
    }

    private func writeMemory(into output: inout String) {
        let pageSize = 64 * 1024

        let pages =
            if let (offset, string) = self.strings.last {
                (offset + string.utf8.count + pageSize - 1) / pageSize
            } else { 1 }

        output += "(memory (export \"memory\") \(pages))\n"

        for (offset, string) in self.strings {
            output += "(data (i32.const \(offset)) \(string.debugDescription))\n"
        }
    }

    func finish() -> String { self.output }
}
