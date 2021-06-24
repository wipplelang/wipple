import Foundation
import WippleStdlib

let code = CommandLine.arguments[1]
let output = try! run(code)
FileHandle.standardOutput.write(try! JSONEncoder().encode(output))

struct RunOutputResponse: Codable {
    let success: Bool
    let text: String

    init(_ output: RunOutput) {
        let (success, text) = output.components
        self.success = success
        self.text = text
    }
}

func run(_ code: String) throws -> [RunOutputResponse] {
    let output = try WippleStdlib.run(code) { env, stack in
        stack.makeCancellable(afterTimeout: 5)
    }

    return output.map(RunOutputResponse.init)
}
