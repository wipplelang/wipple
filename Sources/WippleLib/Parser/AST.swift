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

public struct AST: Codable {
    public let block: [[AST]]?
    public let list: [AST]?
    public let name: String?
    public let text: String?
    public let number: String?
    public let quoted: Box<AST>?
    public let location: Location
}
