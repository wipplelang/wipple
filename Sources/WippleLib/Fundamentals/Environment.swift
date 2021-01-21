import Foundation

/// The environment contains the program state
public struct Environment {
    public var variables: [String: Value] = [:]
    public internal(set) var conformances: [Conformance] = []
    public var userDefined: [Environment.Key: Value] = [:]
    public var parse: ((_ code: String, _ filePath: String?) throws -> AST)!

    public init() {}
}

public extension Environment {
    struct Key: Hashable, CustomStringConvertible {
        public var id: UUID
        public var debugLabel: String?

        public init(debugLabel: String? = nil) {
            self.id = UUID()
            self.debugLabel = debugLabel
        }

        public var description: String {
            self.debugLabel ?? self.id.description
        }
    }
}

public extension Environment {
    static var global = Environment()
}
