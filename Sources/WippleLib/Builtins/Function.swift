import Foundation

public typealias Function = (Value, inout Environment) throws -> Value

extension TraitID where T == Function {
    static let function = Self(debugLabel: "Function")
}

public extension Trait {
    static func function(_ function: @escaping Function) -> Trait<Function> {
        .init(id: .function) { _ in
            function
        }
    }
}

public extension Value {
    func call(with parameter: Value, _ env: inout Environment) throws -> Value {
        guard let function = try self.traitIfPresent(.function, &env) else {
            throw ProgramError("Cannot call this value because it does not have the Function trait")
        }

        return try function(parameter, &env)
    }
}
