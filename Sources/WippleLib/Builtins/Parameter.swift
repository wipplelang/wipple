import Foundation

public typealias DeclareParameterFunction = (Value, inout Environment) throws -> Void

extension TraitID where T == DeclareParameterFunction {
    static let parameter = Self(debugLabel: "Parameter")
}

public extension Trait {
    static func parameter(_ declareParameter: @escaping DeclareParameterFunction) -> Trait<DeclareParameterFunction> {
        .init(id: .parameter) { _ in
            declareParameter
        }
    }
}
