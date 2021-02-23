import Foundation
import XCTest
@testable import Wipple

class DisplayTests: WippleTestCase {
    func testDisplayQuoted() throws {
        // quoted : 'a
        let quoted = Value(.quoted(Quoted(Value(.name(Name("a"))))))

        let display = try quoted.trait(.text, &self.env, self.stack)
        XCTAssertEqual(display.text, "'a")
    }
}
