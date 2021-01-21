import Foundation
import XCTest
@testable import WippleLib

class OperatorTests: WippleTestCase {
    fileprivate static let operators = [
        ":": Operator(
            precedence: 1,
            arity: .variadic,
            associativity: .right,
            call: ":"
        ),
    ]

    func testOperatorParsing() {
        // TODO: Test the following cases:
        // (setting up test coverage might be a good idea)
        // (instead of writing lots of code for each test case, write a
        // function that accepts strings like below)

        // INPUT            OUTPUT

        // +                +
        test("+",           .success(.c("+")))

        // a + b            (+ a b)
        test("a + b",       .success(.l([.c("+"), .n("a"), .n("b")])))

        // a + b + c        (+ (+ a b) c)
        test("a + b + c",   .success(.l(.c("+"), .l(.c("+"), .n("a"), .n("b")), .n("c"))))

        // a -> b -> c      (-> a (-> b c))
        test("a -> b -> c", .success(.l(.c("->"), .n("a"), .l(.c("->"), .n("b"), .n("c")))))

        // a b -> c         (-> (a b) c)
        test("a b -> c",    .success(.l(.c("->"), .l(.n("a"), .n("b")), .n("c"))))

        // a -> b c         (-> a (b c))
        test("a -> b c",    .success(.l(.c("->"), .n("a"), .l(.n("b"), .n("c")))))

        // a -> b + c       (-> a (+ b c))
        test("a -> b c",    .success(.l(.c("->"), .n("a"), .l(.c("+"), .n("b"), .n("c")))))

        // a + b -> c       (-> (+ a b) c)
        test("a + b -> c",  .success(.l(.c("->"), .l(.c("+"), .n("a"), .n("b")), .n("c"))))

        // a +              ParseOperatorsError.missingBinaryRight
        test("a +",         .failure(.missingBinaryRight))

        // + a              ParseOperatorsError.missingBinaryLeft
        test("+ a",         .failure(.missingBinaryLeft))

        // f ->             ParseOperatorsError.missingVariadicRight
        test("f ->",        .failure(.missingVariadicRight))

        // -> f             ParseOperatorsError.missingVariadicLeft
        test("-> f",        .failure(.missingVariadicLeft))

        // TODO: In the future, treat the four errors above as partial applications instead
    }
}

private enum TestResult: Equatable, CustomStringConvertible {
    case n(_ name: String)
    case c(_ name: String)
    case l(_ list: [TestResult])

    static func l(_ list: TestResult...) -> TestResult {
        .l(list)
    }

    var description: String {
        switch self {
        case let .n(n):
            return "#\(n)"
        case let .c(n):
            return "@\(n)"
        case let .l(list):
            return "(\(list.map(\.description).joined(separator: " ")))"
        }
    }
}

private func test(_ input: String, _ expected: Result<TestResult, ParseOperatorsError>, file: StaticString = #filePath, line: UInt = #line) {
    let list = input.split(separator: " ").map(String.init)

    let operators = findOperators(in: list, getOperator: { name in
        OperatorTests.operators[name]
    })

    let result = parseOperators(
        in: list.map(TestResult.n),
        using: operators,
        groupCall: TestResult.c,
        groupList: TestResult.l
    )

    XCTAssertEqual(result, expected, file: file, line: line)
}
