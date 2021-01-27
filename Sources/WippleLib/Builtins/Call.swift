import Foundation

public typealias CallFunction = (Value, inout Environment) throws -> Value

extension Trait.ID {
    static let call = Self(debugLabel: "Call (internal)")
}

public extension Trait {
    static func call(_ call: @escaping CallFunction) -> Trait {
        Trait(id: .call) { _ in
            call
        }
    }
}

public extension Value {
    func callValue(_ env: inout Environment) throws -> CallFunction {
        try Trait.value(.call, in: self, &env)
    }

    func callValueIfPresent(_ env: inout Environment) throws -> CallFunction? {
        try Trait.value(.call, ifPresentIn: self, &env)
    }

    func call(with parameter: Value, _ env: inout Environment) throws -> Value {
        guard let call = try self.callValueIfPresent(&env) else {
            throw ProgramError("Cannot call this value because it does not have the Call trait")
        }

        return try call(parameter, &env)
    }
}
