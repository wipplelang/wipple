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
        try Trait.find(.call, in: self, &env).value(&env) as! CallFunction
    }

    func call(with parameter: Value, _ env: inout Environment) throws -> Value {
        guard let callTrait = try Trait.find(.call, ifPresentIn: self, &env) else {
            throw ProgramError("Cannot call this value because it does not have the Call trait")
        }

        let call = try callTrait.value(&env) as! CallFunction

        return try call(parameter, &env)
    }
}
