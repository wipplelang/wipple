import Foundation
import XCTest
@testable import WippleLib

class VariableTests: WippleTestCase {
    func testVariableResolution() throws {
        self.env.variables["foo"] = Value()

        let name = Value.assoc(.name("foo"))

        XCTAssertNoThrow(try name.evaluate(&self.env))
    }

    // TODO: Test computed variables
}
