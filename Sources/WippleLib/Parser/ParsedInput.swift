import Foundation

public class Box<T: Codable>: Codable {
    let value: T

    init(_ value: T) {
        self.value = value
    }

    required public init(from decoder: Decoder) throws {
        self.value = try T(from: decoder)
    }

    public func encode(to encoder: Encoder) throws {
        try self.value.encode(to: encoder)
    }
}

public struct ParsedInput: Codable {
    public let block: [[ParsedInput]]?
    public let list: [ParsedInput]?
    public let name: String?
    public let text: String?
    public let number: String?
    public let quoted: Box<ParsedInput>?
    public let location: Location
}
