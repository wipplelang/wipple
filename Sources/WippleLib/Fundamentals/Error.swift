import Foundation

/// An error occurring in the program
public struct ProgramError: Error {
    public var message: String
    public var location: Location?

    public init(_ message: String, location: Location? = nil) {
        self.message = message
        self.location = location
    }
}
