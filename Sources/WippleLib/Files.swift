import Foundation

public func parseAndEvaluate(code: String, filePath: String?, _ env: inout Environment) throws -> Value {
    let ast = try env.parse(code, filePath)
    let value = try ast.convertToValue().evaluate(&env)
    return value
}
