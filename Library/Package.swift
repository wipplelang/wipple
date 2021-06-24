// swift-tools-version:5.4

import PackageDescription

let package = Package(
    name: "WippleStdlib",
    products: [
        .library(
            name: "WippleStdlib",
            targets: ["WippleStdlib"]
        ),
    ],
    dependencies: [
        .package(
            url: "https://github.com/wickwirew/Runtime",
            from: "2.2.0"
        ),
        .package(
            name: "Wipple",
            path: "../Language"
        ),
        .package(
            name: "WippleParser",
            path: "../Parser"
        ),
    ],
    targets: [
        .target(
            name: "WippleStdlib",
            dependencies: ["Runtime", "Wipple", "WippleParser"],
            path: "Sources"
        ),
        .testTarget(
            name: "WippleStdlibTests",
            dependencies: ["WippleStdlib"],
            path: "Tests"
        ),
    ]
)
