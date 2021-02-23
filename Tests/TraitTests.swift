import Foundation
import XCTest
@testable import Wipple

class TraitTests: WippleTestCase {
    func testDirectlyDefinedTrait() throws {
        let trait = Trait(id: .init()) { _, _ in 42 }

        let value = Value(trait)

        XCTAssertEqual(try value.trait(trait.id, &self.env, self.stack), 42)
    }

    func testDerivedTrait() throws {
        let A = Trait(id: .init(debugLabel: "A")) { _, _ in 42 }
        let B = Trait(id: .init(debugLabel: "B")) { _, _ in "hi" }

        let value = Value(A).add(B)

        self.env.addConformance(Conformance(
            derivedTraitID: B.id,
            validation: A.id.validation,
            deriveTraitValue: { value, _, _ in "hi" }
        ))

        let derivedTrait = try value.trait(B.id, &self.env, self.stack)
        XCTAssertEqual(derivedTrait, "hi")
    }

    func testTraitValidation() throws {
        let n: Double = 42

        let value = Value(.number(Number(n)))
        let validation = TraitID.number.validation

        let result = try validation(value, &self.env, self.stack)
        XCTAssert(result.map(\.number).isValid(equalTo: n))
    }
}
