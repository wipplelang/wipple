public struct ProgramError: Error {
    public var message: String
    public var stack: ProgramStack

    public init(
        message: String,
        _ stack: ProgramStack
    ) {
        self.message = message
        self.stack = stack
    }
}

public struct ProgramStack {
    public var items: [Item]
    private var queuedLocation: SourceLocation?
    private var recordingEnabled: Bool

    public init() {
        self.items = []
        self.queuedLocation = nil
        self.recordingEnabled = true
    }
}

extension ProgramStack {
    public mutating func queueLocation(_ location: SourceLocation) {
        self.queuedLocation = location
    }

    public mutating func disableRecording() {
        self.recordingEnabled = false
    }

    public func add(_ item: @autoclosure () -> Item) -> ProgramStack {
        guard self.recordingEnabled else {
            return self
        }

        var item = item()
        if item.location == nil {
            item.location = self.queuedLocation
        }
        
        #if WIPPLE_LOG_DIAGNOSTICS
        print("\(String(repeating: "  ", count: self.items.count))\(item.label)")
        #endif
        
        var stack = self
        stack.items.append(item)
        stack.queuedLocation = nil
        stack.recordingEnabled = true

        return stack
    }

    public func add(
        _ label: @autoclosure () -> String,
        location: SourceLocation? = nil
    ) -> ProgramStack {
        self.add(Item(label: label(), location: location))
    }
}

extension ProgramStack {
    public struct Item {
        public var label: String
        public var location: SourceLocation?

        public init(
            label: String,
            location: SourceLocation? = nil
        ) {
            self.label = label
            self.location = location
        }
    }
}

public struct SourceLocation: Equatable, Hashable, Codable {
    public var file: String?
    public var line: UInt
    public var column: UInt

    public init(
        file: String?,
        line: UInt,
        column: UInt
    ) {
        self.file = file
        self.line = line
        self.column = column
    }
}

extension SourceLocation: CustomStringConvertible {
    public var description: String {
        if let file = self.file {
            return "\(file):\(self.line):\(self.column)"
        } else {
            return "\(self.line):\(self.column)"
        }
    }
}
