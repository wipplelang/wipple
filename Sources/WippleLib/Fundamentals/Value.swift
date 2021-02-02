import Foundation

/// Values are collections of traits
public struct Value {
    public internal(set) var traits: Set<AnyTrait>

    // TODO: Store value's location in 'Location' trait
    public var location: Location?

    public init(location: Location? = nil) {
        self.traits = []
        self.location = location
    }
}

public struct Location: Hashable, Codable {
    public var file: String?
    public var line: UInt
    public var column: UInt

    public init(file: String?, line: UInt, column: UInt) {
        self.file = file
        self.line = line
        self.column = column
    }
}
