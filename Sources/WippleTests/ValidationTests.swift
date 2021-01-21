import Foundation
import XCTest
@testable import WippleLib

class ValidationTests: WippleTestCase {
    func testValidation() throws {
        let validation: Validation = { value, env in
            .valid(newValue: 42)
        }

        let result = try validation("hello", &self.env)
        XCTAssert(result.isValid(equalTo: 42))
    }
}
