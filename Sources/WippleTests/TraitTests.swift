import Foundation
import XCTest
@testable import WippleLib

class TraitTests: WippleTestCase {
    func testDirectlyDefinedTrait() throws {
        let trait = Trait(id: .init()) { _ in Void() }

        let value = Value.new(trait)

        XCTAssertEqual(try value.findTrait(trait.id, &self.env), trait)
    }

    func testDerivedTrait() throws {
        let A = Trait(id: .init(debugLabel: "A")) { _ in Void() }
        let B = Trait(id: .init(debugLabel: "B")) { _ in Void() }

        let value = Value
            .new(A)
            .add(B)

        self.env.addConformance(
            derivedTraitID: B.id,
            validation: A.id.validation(),
            deriveTraitValue: { value, env in value }
        )

        let derivedTrait = try value.findTrait(B.id, &env)
        XCTAssertEqual(derivedTrait, B)
    }

    func testTraitValidation() throws {
        let n: Decimal = 42

        let value = Value.new(.number(n))
        let validation = TraitID.number.validation()

        let result = try validation(value, &self.env)
        XCTAssert(result.isValid(equalTo: n))
    }
}
