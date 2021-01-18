import Foundation

public typealias List = [Value]

extension Trait.ID {
    static let list = Self(debugLabel: "List")
}

public extension Trait {
    static func list(_ list: List) -> Trait {
        Trait(id: .list) { _ in
            list
        }
    }
}

public extension Value {
    func listValue(_ env: inout Environment) throws -> List {
        try Trait.find(.list, in: self, &env).value(&env) as! List
    }
}

// MARK: - Initialize

public func initializeList(_ env: inout Environment) {
    // (List and (each Display)) ::= Display
    // TODO: Write this in Wipple code
    env.addConformance(
        derivedTraitID: .display,
        validation: Trait.validation(for: .list) && { value, env in
            let list = value as! List

            var displays: [String] = []
            for value in list {
                guard let display = try Trait.find(.display, ifPresentIn: value, &env) else {
                    return .invalid
                }

                displays.append(try display.value(&env) as! String)
            }

            return .valid(newValue: displays)
        },
        deriveTraitValue: { value, env in
            let displays = value as! [String]

            return "(\(displays.joined(separator: " ")))"
        }
    )
}
