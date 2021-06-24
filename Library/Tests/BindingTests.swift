import XCTest
import Wipple
@testable import WippleStdlib
import Runtime

class BindingTests: XCTestCase {
    struct Person: Equatable, ValueConvertible {
        let name: String
        let favoriteColor: Color
    }

    enum Color: Equatable, ValueConvertible {
        case red
        case green
        case blue
    }

    func testFromValue() throws {
        let value = try Value(Module(capturing: Env.global.child { env in
            try env.setVariable("name", to: Value(Text(rawValue: "Bob")), Stack())
            try env.setVariable("favorite-color", to: Value(Variant(in: VariantSet(enum: Color.self), name: "Green")), Stack())
        }))

        let person = try Person.fromValue(value, Env.global, Stack())

        XCTAssertEqual(person, Person(name: "Bob", favoriteColor: .green))
    }

    func testToValue() throws {
        let value = try Person(name: "Bob", favoriteColor: .green).toValue(Env.global, Stack())

        guard let module = value.primitiveValue as? Module else {
            return XCTFail()
        }

        let name = try Name(rawValue: "name").resolve(module.capturedEnv, Stack())
        guard let number = name.primitiveValue as? Text else { return XCTFail() }
        XCTAssertEqual(number.rawValue, "Bob")

        let favoriteColor = try Name(rawValue: "favorite-color").resolve(module.capturedEnv, Stack())
        guard let variant = favoriteColor.primitiveValue as? Variant else { return XCTFail() }
        XCTAssert(variant.belongs(to: VariantSet(enum: Color.self)))
        XCTAssertEqual(variant.name, "Green")
    }

    func testBoundFunction() throws {
        func add(_ a: Int, _ b: Int) -> Int { a + b }

        let output: String = WippleStdlib.run("show (add 2 2)", prepare: { env, stack in
            try env.setVariable("add", to: WippleStdlib.bind(add), stack)
        })

        XCTAssertEqual(output, "4")
    }

    func testBoundComputedVariable() throws {
        var success = false

        WippleStdlib.run("success!", prepare: { env, stack in
            let test = WippleStdlib.bind {
                success = true
            }

            try env.setComputedVariable("success!", to: test, stack)
        })

        XCTAssertTrue(success)
    }
}

