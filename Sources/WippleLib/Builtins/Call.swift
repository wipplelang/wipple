import Foundation

public typealias CallFunction = (Value, inout Environment) throws -> Value

extension TraitID where T == CallFunction {
    static let call = Self(debugLabel: "Call (internal)")
}

public extension Trait {
    static func call(_ call: @escaping CallFunction) -> Trait<CallFunction> {
        .init(id: .call) { _ in
            call
        }
    }
}

public extension Value {
    func call(with parameter: Value, _ env: inout Environment) throws -> Value {
        guard let call = try self.traitIfPresent(.call, &env) else {
            throw ProgramError("Cannot call this value because it does not have the Call trait")
        }

        return try call(parameter, &env)
    }
}
