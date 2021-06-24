#if os(macOS)
    private let os = "macos"
#elseif os(Linux)
    private let os = "linux"
#else
    #error("Unsupported platform")
#endif

#if arch(x86_64)
    private let arch = "x86_64"
#elseif arch(arm64)
    private let arch = "arm64"
#else
    #error("Unsupported platform")
#endif

public let platform = "\(arch)-\(os)"
