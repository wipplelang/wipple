import Foundation
import XCTest
@testable import Wipple

class ValidationTests: WippleTestCase {
    func testValidation() throws {
        let validation = Validation<String, Int> { value, env, stack in
            .valid(42)
        }

        let result = try validation("hello", &self.env, self.stack)
        XCTAssert(result.isValid(equalTo: 42))
    }
}
