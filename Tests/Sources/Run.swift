import ArgumentParser
import Rainbow
import Foundation
import Wipple
import WippleStdlib

@main struct Run: ParsableCommand {
    @Option(name: .customLong("file"))
    var singleFile: String?

    @Option(name: .customLong("test"))
    var singleTest: String?

    func run() throws {
        var testFiles = loadTests()

        if let fileName = self.singleFile {
            testFiles = testFiles.filter { $0.name == fileName }
        }

        if let testName = self.singleTest {
            testFiles = testFiles.compactMap { file in
                var file = file
                file.tests = file.tests.filter { $0.name == testName }
                return file.tests.isEmpty ? nil : file
            }
        }

        var passCount = 0
        var failCount = 0

        var space = false

        for file in testFiles {
            if space {
                print()
            }

            print("\(file.name.bold.underline) \(file.path.lastPathComponent.dim)\n")

            for test in file.tests {
                let result = try test.run()

                func durationText(_ duration: TimeInterval) -> String {
                    "\(Int(duration * 1000))ms".dim
                }

                func indent(_ string: String) -> String {
                    let indent = String(repeating: " ", count: 8)

                    return string
                        .split(separator: "\n", omittingEmptySubsequences: false)
                        .map { indent + $0 }
                        .joined(separator: "\n")
                }

                switch result {
                case .passed(let duration):
                    print("\("PASS".bold.green) \(test.name) \(durationText(duration))")

                    passCount += 1
                case .failed(let expected, let found, let duration):
                    if space {
                        print()
                    }

                    print("""
                    \("FAIL".bold.red) \(test.name) \(durationText(duration))
                        Expected:
                    \(indent(expected).dim)

                        Found:
                    \(indent(found).dim)
                    """)

                    if space {
                        print()
                    }

                    failCount += 1
                }

                space = true
            }
        }

        func countText(_ count: Int, _ type: String, color: Color) -> String {
            let string = "\(count) \(type)"
            return count > 0 ? string.applyingColor(color) : string
        }

        if space {
            print()
        }

        print("\(passCount + failCount) tests, \(countText(passCount, "passed", color: .green)), \(countText(failCount, "failed", color: .red))".bold)

        if failCount > 0 {
            throw ExitCode.failure
        }
    }
}
