import Foundation

public func initialize(_ env: inout Environment) {
    initializeBlock(&env)
    initializeEmpty(&env)
    initializeEnvironmentContainer(&env)
    initializeEvaluation(&env)
    initializeList(&env)
    initializeName(&env)
    initializeNumber(&env)
    initializeQuoted(&env)
    initializeTraitConstructor(&env)
}
