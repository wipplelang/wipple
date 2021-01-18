import Foundation
import XCTest
@testable import WippleLib

class TraitTests: XCTestCase {
    var env: Environment!

    override func setUp() {
        self.env = Environment()
        WippleLib.initialize(&self.env)
    }

    func testDirectlyDefinedTrait() throws {
        let trait = Trait(id: .init()) { _ in Void() }

        let value = Value.assoc(trait)

        XCTAssertEqual(try Trait.find(trait.id, in: value, &self.env), trait)
    }

    func testDerivedTrait() throws {
        let A = Trait(id: .init(debugLabel: "A")) { _ in Void() }
        let B = Trait(id: .init(debugLabel: "B")) { _ in Void() }

        let value = Value
            .assoc(A)
            .trait(B)

        self.env.addConformance(
            derivedTraitID: B.id,
            validation: Trait.validation(for: A.id),
            deriveTraitValue: { value, env in value }
        )

        let derivedTrait = try Trait.find(B.id, in: value, &self.env)
        XCTAssertEqual(derivedTrait, B)

        let derivedTraitValue = try derivedTrait.value(&self.env)
        XCTAssertNotNil(derivedTraitValue as? Void)
    }
}
