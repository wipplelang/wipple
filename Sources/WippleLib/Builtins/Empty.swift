import Foundation

func initializeEmpty(_ env: inout Environment) {
    env.variables["empty"] = Value()
}
