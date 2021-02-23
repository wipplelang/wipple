public typealias MacroParameter = String

public typealias DefineMacroParameterFunc = (Value, inout Environment, ProgramStack) throws -> (
    MacroParameter, Value
)

extension TraitID where T == DefineMacroParameterFunc {
    public static let macroParameter = TraitID(debugLabel: "Macro-Parameter")
}

extension Trait where T == DefineMacroParameterFunc {
    public static func macroParameter(_ value: @escaping DefineMacroParameterFunc) -> Self {
        Trait(id: .macroParameter) { _, _ in value }
    }
}

public typealias MacroExpandFunc = (MacroParameter, Value, inout Environment, ProgramStack) throws
    -> Value

extension TraitID where T == MacroExpandFunc {
    public static let macroExpand = TraitID(debugLabel: "Macro-Expand")
}

extension Trait where T == MacroExpandFunc {
    public static func macroExpand(_ value: @escaping MacroExpandFunc) -> Self {
        Trait(id: .macroExpand) { _, _ in value }
    }
}

extension Value {
    public func macroExpand(
        parameter: MacroParameter,
        replacement: Value,
        _ env: inout Environment,
        _ stack: ProgramStack
    ) throws -> Value {
        let stack =
            stack
            .add(
                "Expanding '\(self.format(&env, stack))', replacing '\(parameter)' with '\(replacement.format(&env, stack))'"
            )

        guard let macroExpand = try self.traitIfPresent(.macroExpand, &env, stack) else {
            return self
        }

        return try macroExpand(parameter, replacement, &env, stack)
    }
}

public struct Macro {
    public var defineParameter: DefineMacroParameterFunc
    public var valueToExpand: Value

    public init(
        defineParameter: @escaping DefineMacroParameterFunc,
        valueToExpand: Value
    ) {
        self.defineParameter = defineParameter
        self.valueToExpand = valueToExpand
    }
}

extension TraitID where T == Macro {
    public static let macro = TraitID(debugLabel: "Macro")
}

extension Trait where T == Macro {
    public static func macro(_ value: Macro) -> Self {
        Trait(id: .macro) { _, _ in value }
    }
}

internal func setupMacro(_ env: inout Environment) {
    // Macro ::= Function
    env.addConformance(
        Conformance(
            derivedTraitID: .function,
            validation: TraitID.macro.validation,
            deriveTraitValue: { macro, _, _ in
                { value, env, stack in
                    let (parameter, replacement) = try macro.defineParameter(value, &env, stack)

                    return try macro.valueToExpand.macroExpand(
                        parameter: parameter,
                        replacement: replacement,
                        &env,
                        stack
                    )
                }
            }
        )
    )

    // Macro ::= Text
    env.addConformance(
        Conformance(
            derivedTraitID: .text,
            validation: TraitID.macro.validation,
            deriveTraitValue: { _, _, _ in
                Text("<macro>")
            }
        )
    )
}
