import Foundation

public struct SourceFile: Hashable, CustomDebugStringConvertible {
    public enum DisplayMode: Hashable {
        case fullPath
        case fileName
        case hidden
    }

    public var path: URL
    public var displayMode: DisplayMode

    public init(path: URL, displayMode: DisplayMode = .fullPath) {
        self.path = path
        self.displayMode = displayMode
    }

    public var description: String? {
        switch self.displayMode {
        case .fullPath:
            return self.path.path
        case .fileName:
            return self.path.lastPathComponent
        case .hidden:
            return nil
        }
    }

    public var debugDescription: String {
        self.description ?? self.path.debugDescription
    }
}

public struct SourceLocation: Hashable, CustomStringConvertible, Primitive {
    public var file: SourceFile?
    public var line: Int
    public var column: Int

    public init(file: SourceFile? = nil, line: Int, column: Int) {
        self.file = file
        self.line = line
        self.column = column
    }

    public init(file: SourceFile?) {
        self.init(file: file, line: 1, column: 1)
    }

    public var description: String {
        if let file = self.file?.description {
            return "\(file):\(self.line):\(self.column)"
        } else {
            return "\(self.line):\(self.column)"
        }
    }
}

public extension Value {
    var location: SourceLocation? {
        self.instances(of: SourceLocation.self).last
    }

    mutating func addLocation(_ location: SourceLocation) {
        self.addAttribute(value: location)
    }
}

public struct DiagnosticsStack {
    public var items: [Item]
    private var queuedLocation: SourceLocation?
    private var recordingEnabled: Bool
}

public extension DiagnosticsStack {
    init() {
        self.items = []
        self.queuedLocation = nil
        self.recordingEnabled = true
    }
}

public extension DiagnosticsStack {
    mutating func queue(_ location: SourceLocation?) {
        if let location = location {
            self.queuedLocation = location
        }
    }

    mutating func disableRecording() {
        self.recordingEnabled = false
    }

    mutating func add(_ item: @autoclosure () -> Item) {
        guard self.recordingEnabled else { return }

        var item = item()
        if item.location == nil {
            item.location = self.queuedLocation
        }

        self.items.append(item)
        self.queuedLocation = nil
        self.recordingEnabled = true
    }

    mutating func add(_ label: @autoclosure () -> String, location: SourceLocation? = nil) {
        self.add(Item(label: label(), location: location))
    }
}

public extension DiagnosticsStack {
    struct Item {
        public var label: String
        public var location: SourceLocation?
    }
}

extension DiagnosticsStack.Item: CustomStringConvertible {
    public var description: String {
        if let location = self.location {
            return "\(self.label) (\(location))"
        } else {
            return self.label
        }
    }
}

public extension Stack {
    private struct DiagnosticsKey: StackKey {
        typealias Value = DiagnosticsStack
    }

    var diagnostics: DiagnosticsStack {
        get { self[DiagnosticsKey.self] ?? DiagnosticsStack() }
        set { self[DiagnosticsKey.self] = newValue }
    }
}

public struct CancelationReason {
    public let message: String

    public init(message: String) {
        self.message = message
    }

    public static let unspecified = CancelationReason(message: "Execution was canceled")
    public static let timeout = CancelationReason(message: "Execution timed out")
}

public extension Stack {
    private class CancelSignal {
        let shouldCancel: () -> CancelationReason?

        init(shouldCancel: @escaping () -> CancelationReason?) {
            self.shouldCancel = shouldCancel
        }
    }

    private struct CancelSignalKey: StackKey {
        typealias Value = CancelSignal
    }

    private var cancelSignal: CancelSignal? {
        get { self[CancelSignalKey.self] }
        set { self[CancelSignalKey.self] = newValue }
    }

    mutating func makeCancellable(shouldCancel: @escaping () -> Bool) {
        self.makeCancellable { shouldCancel() ? .unspecified : nil }
    }

    mutating func makeCancellable(afterTimeout timeout: TimeInterval, start: Date = Date()) {
        self.makeCancellable { -start.timeIntervalSinceNow > timeout ? .timeout : nil }
    }

    mutating func makeCancellable(shouldCancel: @escaping () -> CancelationReason?) {
        self.cancelSignal = CancelSignal(shouldCancel: shouldCancel)
    }

    var cancelationReason: CancelationReason? {
        guard let cancelSignal = cancelSignal else {
            return nil
        }

        return cancelSignal.shouldCancel()
    }
}

public enum Exit: Swift.Error {
    case `return`(Value, Stack)
    case `break`(Value, Stack)
    case cancel(CancelationReason, Stack)
    case error(Error)

    public static func error(_ message: String, _ stack: Stack) -> Exit {
        Exit.error(Error(message, stack.diagnostics))
    }
}

public func cancelIfSignaled(_ stack: Stack) throws {
    if let reason = stack.cancelationReason {
        throw Exit.cancel(reason, stack)
    }
}

public func catchReturn(perform: () throws -> Value) rethrows -> Value {
    do {
        return try perform()
    } catch let exit as Exit {
        if case .return(let value, _) = exit {
            return value
        }

        throw exit
    }
}

public struct Error {
    public let message: String
    public let stack: DiagnosticsStack

    public init(_ message: String, _ stack: DiagnosticsStack) {
        self.message = message
        self.stack = stack
    }
}

public extension Exit {
    var error: Error {
        switch self {
        case .return(_, let stack):
            return Error("'return' from top level", stack.diagnostics)
        case .break(_, let stack):
            return Error("'break' from top level", stack.diagnostics)
        case .cancel(let reason, let stack):
            return Error(reason.message, stack.diagnostics)
        case .error(let error):
            return error
        }
    }
}

extension Error: CustomStringConvertible {
    public var description: String {
        let stack = self.stack.items
            .reversed()
            .map { "    \($0)" }
            .joined(separator: "\n")

        return "\(self.message)\n\(stack)"
    }
}
