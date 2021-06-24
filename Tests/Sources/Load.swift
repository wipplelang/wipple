import Foundation
import Regex
import SyntaxTree
import Wipple
import WippleStdlib

struct TestFile {
    var path: URL
    var name: String
    var tests: [Test]
}

struct Test {
    var name: String
    var line: Int
    var run: () throws -> TestResult
}

enum TestResult {
    case passed(duration: TimeInterval)
    case failed(expected: String, found: String, duration: TimeInterval)
}

private let regex = try! Regex(
    pattern: #"""
    >>>\ ([^\n]*\n*)     # test name
    ((?:.(?!\n---))*)    # test code
    \n*---\n*            # separator
    ((?:.(?!\n>>>\ ))*)  # expected output
    """#,
    options: [.allowCommentsAndWhitespace, .dotMatchesLineSeparators],
    groupNames: ["name", "code", "output"]
)

func loadTests() -> [TestFile] {
    let testFiles = Bundle.module.urls(forResourcesWithExtension: "wpl", subdirectory: "Resources")!

    return testFiles
        .map { file in
            let fileName = file
                .deletingPathExtension()
                .lastPathComponent
                .replacingOccurrences(of: "-", with: " ")
                .capitalized

            let code = try! String(contentsOf: file)
            let index = LineColumnIndex(string: code)

            let tests: [Test] = regex.findAll(in: code).map { match in
                let line = index[code.distance(from: code.startIndex, to: match.range.lowerBound)]!.line

                let capture = { match.group(named: $0)!.trimmingCharacters(in: .whitespacesAndNewlines) }

                let name = capture("name")
                let code = capture("code")
                let expectedOutput = capture("output")

                return Test(name: name, line: line) {
                    try test(
                        code,
                        file: .init(path: file, displayMode: .hidden),
                        expectedOutput: expectedOutput
                    )
                }
            }

            return TestFile(path: file, name: fileName, tests: tests)
        }
        .sorted(by: { $0.name < $1.name })
}

func test(_ code: String, file: SourceFile, expectedOutput: String) throws -> TestResult {
    let start = Date()

    let output = try run(code, file: file)
        .map(\.description)
        .joined(separator: "\n")

    let duration = -start.timeIntervalSinceNow

    return output == expectedOutput
        ? .passed(duration: duration)
        : .failed(expected: expectedOutput, found: output, duration: duration)
}
