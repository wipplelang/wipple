import Foundation
import XCTest
@testable import Wipple

class WippleTestCase: XCTestCase {
    var env: Environment!
    var stack: ProgramStack!

    override func setUp() {
        super.setUp()

        XCTAssertNoThrow(self.env = try Wipple.setup())
        self.stack = ProgramStack()
    }
}

extension ValidationResult where T: Equatable {
    func isValid(equalTo other: T) -> Bool {
        switch self {
        case .valid(let value):
            return value == other
        case .invalid:
            return false
        }
    }
}
