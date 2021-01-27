import Foundation
import XCTest
@testable import WippleLib

class WippleTestCase: XCTestCase {
    var env: Environment!

    override func setUp() {
        super.setUp()

        self.env = Environment()

        XCTAssertNoThrow(try WippleLib.initialize(&self.env))
    }
}
