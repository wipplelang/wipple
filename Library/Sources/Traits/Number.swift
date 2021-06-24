import Foundation
import Wipple

public struct Number: RawRepresentable, Primitive {
    public var rawValue: Decimal

    public init(rawValue: Decimal) {
        self.rawValue = rawValue
    }
}

internal func setupNumber(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Number", to: Value(Trait(Number.self)), stack)

    // Number == Text
    try env.addRelation(from: Number.self, to: Text.self, stack) { number in
        Text(rawValue: "\(number.rawValue)")
    }
}
