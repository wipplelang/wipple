import Foundation
import WippleLib

struct Options: Codable {
    let filePath: String?
    let code: String
    let parserPath: String
    let printResult: Bool
}

do {
    let options = try JSONDecoder().decode(Options.self, from: CommandLine.arguments[1].data(using: .utf8)!)

    try initialize(&.global)

    Environment.global.parse = { code, filePath in
        let output = shell(options.parserPath, filePath ?? "", code)
        let ast = try JSONDecoder().decode(AST.self, from: output.data(using: .utf8)!)
        return ast
    }

    var fileEnv = Environment.global

    let result = try parseAndEvaluate(code: options.code, filePath: options.filePath, &fileEnv)

    if options.printResult {
        let display = try result.traitIfPresent(.text, &fileEnv) ?? "<value>"
        print(display)
    }
} catch {
    var s = FileHandle.standardError
    print(error, to: &s)
    exit(1)
}

extension FileHandle: TextOutputStream {
    public func write(_ string: String) {
        self.write(string.data(using: .utf8)!)
    }
}
