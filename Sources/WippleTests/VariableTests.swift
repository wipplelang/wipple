import Foundation
import XCTest
@testable import WippleLib

class VariableTests: XCTestCase {
    var env: Environment!

    override func setUp() {
        self.env = Environment()
        WippleLib.initialize(&self.env)
    }

    func testVariableResolution() throws {
        self.env.variables["foo"] = Value()

        let name = Value.assoc(.name("foo"))

        XCTAssertNoThrow(try name.evaluate(&self.env))
    }

    // TODO: Test computed variables
}
