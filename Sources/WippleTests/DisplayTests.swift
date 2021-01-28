import Foundation
import XCTest
@testable import WippleLib

class DisplayTests: WippleTestCase {
    func testDisplayQuoted() throws {
        // quoted : 'a
        let quoted = Value.assoc(.quoted(Value.assoc(.name("a"))))

        let display = try quoted.textValueWithDefault(&self.env)
        XCTAssertEqual(display, "'a")
    }
}
