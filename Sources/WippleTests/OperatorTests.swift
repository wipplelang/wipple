import Foundation
import XCTest
@testable import WippleLib

class OperatorTests: WippleTestCase {
    fileprivate static let operators = [
        // base operator
        ":": Operator(
            arity: .variadic,
            associativity: .right,
            call: ":"
        ),

        "->": Operator(
            arity: .variadic,
            associativity: .right,
            call: "->"
        ),

        "+": Operator(
            arity: .binary,
            associativity: .left,
            call: "+"
        ),

        "-": Operator(
            arity: .binary,
            associativity: .left,
            call: "-"
        ),
    ]

    fileprivate static let operatorsByPrecedence: [Set<Operator<String>>] = {
        var p = [Set<Operator<String>>]()

        try! registerOperator(operators[":"]!, precedence: .lowest, in: &p).get()
        try! registerOperator(operators["->"]!, precedence: .higherThan(operators[":"]!), in: &p).get()
        try! registerOperator(operators["+"]!, precedence: .highest, in: &p).get()
        try! registerOperator(operators["-"]!, precedence: .sameAs(operators["+"]!), in: &p).get()

        return p
    }()

    func testRegistering() {
        XCTAssertEqual(Self.operatorsByPrecedence, [
            [
                Self.operators["+"]!,
                Self.operators["-"]!,
            ],
            [
                Self.operators["->"]!,
            ],
            [
                Self.operators[":"]!,
            ],
        ])
    }

    func testParsing() {
        //                  +
        test("+",           .success(.c("+")))

        //                  ((+ a b))
        test("a + b",       .success(.l(.l(.c("+"), .n("a"), .n("b")))))

        //                  (-> (a b) c)
        test("a b -> c",    .success(.l(.c("->"), .l(.n("a"), .n("b")), .n("c"))))

        //                  (-> a (b c))
        test("a -> b c",    .success(.l(.c("->"), .n("a"), .l(.n("b"), .n("c")))))
    }

    func testPrecedence() {
        #warning("TODO: Test Precedence")
    }

    func testAssociativity() {
        //                  ((+ a b) + c)
        test("a + b + c",   .success(.l(.l(.c("+"), .n("a"), .n("b")), .n("+"), .n("c"))))

        //                  (-> a (b -> c))
        test("a -> b -> c", .success(.l(.c("->"), .n("a"), .l(.n("b"), .n("->"), .n("c")))))

        //                  (-> a (b + c))
        test("a -> b + c",  .success(.l(.c("->"), .n("a"), .l(.n("b"), .n("+"), .n("c")))))

        //                  (-> (a + b) c)
        test("a + b -> c",  .success(.l(.c("->"), .l(.n("a"), .n("+"), .n("b")), .n("c"))))
    }

    // TODO: In the future, treat these errors as partial application instead
    func testPartialApplication() {
        //                  ParseOperatorsError.missingBinaryRight
        test("a +",         .failure(.missingBinaryRight))

        //                  ParseOperatorsError.missingBinaryLeft
        test("+ a",         .failure(.missingBinaryLeft))

        //                  ParseOperatorsError.missingVariadicRight
        test("f ->",        .failure(.missingVariadicRight))

        //                  ParseOperatorsError.missingVariadicLeft
        test("-> f",        .failure(.missingVariadicLeft))
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
            return "\(n)"
        case let .c(n):
            return "\(n)"
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
        operatorsInList: operators,
        operatorsByPrecedence: OperatorTests.operatorsByPrecedence,
        groupCall: TestResult.c,
        groupList: TestResult.l
    )

    XCTAssertEqual(result, expected, file: file, line: line)
}
