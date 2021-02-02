import Foundation
import XCTest
@testable import WippleLib

class DisplayTests: WippleTestCase {
    func testDisplayQuoted() throws {
        // quoted : 'a
        let quoted = Value.new(.quoted(Value.new(.name("a"))))

        let display = try quoted.trait(.text, &self.env)
        XCTAssertEqual(display, "'a")
    }
}
