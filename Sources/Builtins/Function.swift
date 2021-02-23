public typealias Function = (Value, inout Environment, ProgramStack) throws -> Value

extension TraitID where T == Function {
    public static let function = TraitID(debugLabel: "Function")
}

extension Trait where T == Function {
    public static func function(_ value: @escaping Function) -> Self {
        Trait(id: .function) { _, _ in value }
    }
}

extension Value {
    public func call(
        with parameter: Value,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> Value {
        let stack = stack.add("Calling '\(self.format(&env, stack))'")

        let function = try self.trait(
            .function,
            orError: "Cannot call this value because it does not have the Function trait",
            &env,
            stack
        )

        return try function(parameter, &env, stack)
    }
}
