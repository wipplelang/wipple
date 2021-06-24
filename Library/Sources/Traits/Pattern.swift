import Wipple

extension Pattern: Primitive {}

internal func setupPattern(_ env: Env, _ stack: Stack) throws {
    try env.setVariable("Pattern", to: Value(Trait(Pattern.self)), stack)

    try env.addRelation(text: "pattern", for: Pattern.self, stack)
}
