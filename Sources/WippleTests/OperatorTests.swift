import Foundation
import XCTest
@testable import WippleLib

class OperatorTests: WippleTestCase {
    func testOperatorParsing() {
        // TODO: Test the following cases:
        // (setting up test coverage might be a good idea)
        // (instead of writing lots of code for each test case, write a
        // function that accepts strings like below)

        // INPUT            OUTPUT

        // +                +

        // a + b            (+ a b)

        // a + b + c        (+ (+ a b) c)

        // a -> b -> c      (-> a (-> b c))

        // a b -> c         (-> (a b) c)

        // a -> b c         (-> a (b c))

        // a -> b + c       (-> a (+ b c))

        // a + b -> c       (-> (+ a b) c)

        // a +              ParseOperatorsError.missingBinaryRight

        // + a              ParseOperatorsError.missingBinaryLeft

        // f ->             ParseOperatorsError.missingVariadicRight

        // -> f             ParseOperatorsError.missingVariadicLeft

        // TODO: In the future, treat the four errors above as partial applications instead
    }
}
