import Foundation
import XCTest
import WippleLib

class NameTests: XCTestCase {
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
}
