protocol Mangleable { var mangled: String { get } }

extension Node: Mangleable { var mangled: String { "_\(self.id)" } }

extension IR.DefinitionKey: Mangleable {
    var mangled: String {
        var mangled = self.node.mangled

        for (parameter, type) in self.substitutions {
            mangled += "\(parameter.mangled)_\(type.nominal)"
        }

        // Bounds do not need to be part of the mangled ID because instances are
        // globally unique

        return mangled
    }
}

extension IR.`Type` {
    var nominal: String { self.mangled(nominal: true)! }

    var structural: String? { self.mangled(nominal: false) }

    private func mangled(nominal: Bool) -> String? {
        switch self {
        case .named(let definition, let parameters, let intrinsic, let representation, _):
            guard nominal || (!intrinsic && representation == nil) else { return nil }

            var mangled = "type\(definition.mangled)"
            for parameter in parameters { mangled += "_\(parameter.nominal)" }
            return mangled
        case .tuple(let elements):
            var mangled = "tuple"
            for element in elements { mangled += "_\(element.nominal)" }
            return mangled
        case .function(let inputs, let output):
            var mangled = "function"
            for input in inputs { mangled += "_\(input.nominal)" }
            mangled += "_\(output.nominal)"
            return mangled
        case .parameter(let definition): return "parameter\(definition.mangled)"
        }
    }
}
