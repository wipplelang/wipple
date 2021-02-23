import Foundation
import XCTest
@testable import Wipple

class VariableTests: WippleTestCase {
    func testVariableResolution() throws {
        self.env.variables["foo"] = .empty

        let name = Value(.name(Name("foo")))

        XCTAssertNoThrow(try name.evaluate(&self.env, self.stack))
    }

    // TODO: Test computed variables
}
