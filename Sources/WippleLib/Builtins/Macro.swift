import Foundation

public typealias DefineMacroParameterFunction = (Value, inout Environment) throws -> (parameter: Name, replacement: Value)

extension TraitID where T == DefineMacroParameterFunction {
    static let macroParameter = Self(debugLabel: "Macro-Parameter")
}

public extension Trait {
    static func macroParameter(_ macroParameter: @escaping DefineMacroParameterFunction) -> Trait<DefineMacroParameterFunction> {
        .init(id: .macroParameter) { _ in
            macroParameter
        }
    }
}

public typealias MacroExpand = (_ parameter: Name, _ replacement: Value, inout Environment) throws -> Value

extension TraitID where T == MacroExpand {
    static let macroExpand = Self(debugLabel: "Macro-Expand")
}

public extension Trait {
    static func macroExpand(_ macroExpand: @escaping MacroExpand) -> Trait<MacroExpand> {
        .init(id: .macroExpand) { _ in
            macroExpand
        }
    }
}

public extension Value {
    func macroExpand(parameter: Name, replacement: Value, _ env: inout Environment) throws -> Value {
        guard let macroExpand = try self.traitIfPresent(.macroExpand, &env) else {
            return self
        }
        
        return try macroExpand(parameter, replacement, &env)
    }
}

public struct Macro {
    public var defineParameter: DefineMacroParameterFunction
    public var valueToExpand: Value
    
    public init(defineParameter: @escaping DefineMacroParameterFunction, valueToExpand: Value) {
        self.defineParameter = defineParameter
        self.valueToExpand = valueToExpand
    }
}

extension TraitID where T == Macro {
    static let macro = Self(debugLabel: "Macro")
}

public extension Trait {
    static func macro(_ macro: Macro) -> Trait<Macro> {
        .init(id: .macro) { _ in
            macro
        }
    }
    
    static func macro(defineParameter: @escaping DefineMacroParameterFunction, valueToExpand: Value) -> Trait<Macro> {
        self.macro(Macro(defineParameter: defineParameter, valueToExpand: valueToExpand))
    }
}

public func initializeMacro(_ env: inout Environment) {
    // Macro ::= Function
    env.addConformance(
        derivedTraitID: .function,
        validation: TraitID.macro.validation(),
        deriveTraitValue: { macro, env in
            return { input, env in
                let (parameter, replacement) = try macro.defineParameter(input, &env)
                
                return try macro.valueToExpand
                    .macroExpand(parameter: parameter, replacement: replacement, &env)
                    .evaluate(&env)
            }
        }
    )
    
    // Macro ::= Text
    // TODO: Implement in Wipple code
    env.addConformance(
        derivedTraitID: .text,
        validation: TraitID.macro.validation(),
        deriveTraitValue: { value, env in
            "<macro>"
        }
    )
}
