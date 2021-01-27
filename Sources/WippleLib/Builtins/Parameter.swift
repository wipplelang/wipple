import Foundation

public typealias DeclareParameterFunction = (Value, inout Environment) throws -> Void

extension Trait.ID {
    static let parameter = Self(debugLabel: "Parameter")
}

public extension Trait {
    static func parameter(_ declareParameter: @escaping DeclareParameterFunction) -> Trait {
        Trait(id: .parameter) { _ in
            declareParameter
        }
    }
}

public extension Value {
    func parameterValue(_ env: inout Environment) throws -> DeclareParameterFunction {
        try Trait.value(.parameter, in: self, &env)
    }

    func parameterValueIfPresent(_ env: inout Environment) throws -> DeclareParameterFunction? {
        try Trait.value(.parameter, ifPresentIn: self, &env)
    }
}
