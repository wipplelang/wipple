// swift-tools-version:5.4

import PackageDescription

let package = Package(
    name: "WippleTests",
    products: [
        .executable(
            name: "WippleTests",
            targets: ["WippleTests"]
        ),
    ],
    dependencies: [
        .package(
            url: "https://github.com/onevcat/Rainbow",
            from: "4.0.0"
        ),
        .package(
            url: "https://github.com/crossroadlabs/Regex",
            from: "1.2.0"
        ),
        .package(
            url: "https://github.com/apple/swift-argument-parser",
            .upToNextMinor(from: "0.4.0")
        ),
        .package(
            name: "WippleStdlib",
            path: "../Library"
        ),
    ],
    targets: [
        .executableTarget(
            name: "WippleTests",
            dependencies: [
                "Rainbow",
                "Regex",
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
                "WippleStdlib",
            ],
            path: "Sources",
            resources: [.copy("Resources")]
        ),
    ]
)
