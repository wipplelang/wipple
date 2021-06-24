import Wipple

internal func setupBuiltins(_ env: Env, _ stack: Stack) throws {
    try setupAttribute(env, stack)
    try setupBlock(env, stack)
    try setupEmpty(env, stack)
    try setupEvaluate(env, stack)
    try setupFunction(env, stack)
    try setupList(env, stack)
    try setupLiteral(env, stack)
    try setupMath(env, stack)
    try setupModule(env, stack)
    try setupName(env, stack)
    try setupNumber(env, stack)
    try setupOperator(env, stack)
    try setupPattern(env, stack)
    try setupReference(env, stack)
    try setupText(env, stack)
    try setupTrait(env, stack)
    try setupVariant(env, stack)
}
