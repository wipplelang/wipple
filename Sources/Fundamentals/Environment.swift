import Foundation

public struct Environment {
    public var variables: [String: Value]
    public var conformances: [AnyConformance]
    public var operatorPrecedences: [PrecedenceGroup]

    private var userDefined: [UUID: Any]

    public init() {
        self.variables = [:]
        self.conformances = []
        self.operatorPrecedences = []
        self.userDefined = [:]
    }
}

extension Environment {
    public struct Key<T>: Equatable, Hashable {
        fileprivate var id: UUID
        public var debugLabel: String?

        public init(
            debugLabel: String? = nil
        ) {
            self.id = UUID()
            self.debugLabel = debugLabel
        }
    }

    public subscript<T>(key: Key<T>) -> T? {
        get { self.userDefined[key.id] as? T }
        set { self.userDefined[key.id] = newValue }
    }
}
