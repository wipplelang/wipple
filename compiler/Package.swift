// swift-tools-version:6.3

import Foundation
import PackageDescription

let package = Package(
    name: "wipple",
    platforms: [.macOS("26")],
    products: [.executable(name: "wipple", targets: ["CLI"])],
    dependencies: [
        .package(url: "https://github.com/apple/swift-argument-parser.git", from: "1.0.0"),
        .package(url: "https://github.com/apple/swift-collections.git", from: "1.0.0"),
        .package(url: "https://github.com/atacan/swift-string-similarity.git", from: "1.0.0"),
        .package(url: "https://github.com/swiftwasm/JavaScriptKit.git", from: "0.50.0"),
    ],
    targets: [
        .executableTarget(
            name: "CLI",
            dependencies: [
                "Compiler", .product(name: "ArgumentParser", package: "swift-argument-parser"),
            ],
            path: "cli",
            resources: [.embedInCode("node-runtime/")],
        ),
        .target(
            name: "Compiler",
            dependencies: [
                .product(name: "Collections", package: "swift-collections"),
                .product(name: "StringSimilarity", package: "swift-string-similarity"),
            ],
            path: "src",
            swiftSettings: [.unsafeFlags(["-O"])],
        ),
        .executableTarget(
            name: "API",
            dependencies: ["Compiler", .product(name: "JavaScriptKit", package: "JavaScriptKit")],
            path: "api",
            swiftSettings: [
                .enableExperimentalFeature("Extern"), .defaultIsolation(MainActor.self),
            ],
            plugins: ProcessInfo.processInfo.environment["WASM_BUILD"] == "1"
                ? [.plugin(name: "BridgeJS", package: "JavaScriptKit")] : [],
        ),
    ],
)
