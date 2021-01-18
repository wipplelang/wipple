import Foundation
import WippleLib

extension FileHandle: TextOutputStream {
    public func write(_ string: String) {
        self.write(string.data(using: .utf8)!)
    }
}

do {
    WippleLib.initialize(&.global)

    var code = ""
    while let line = readLine(strippingNewline: false) {
        code += line
    }

    let parsedInput = try JSONDecoder().decode(ParsedInput.self, from: code.data(using: .utf8)!)

    let value = parsedInput.convertToValue()

    let evaluatedValue = try value.evaluate(&.global) // TODO: Files

    print(evaluatedValue)
} catch {
    var s = FileHandle.standardError
    print(error, to: &s)
    exit(1)
}
