// swift-tools-version:5.4

import PackageDescription

let package = Package(
    name: "WippleParser",
    products: [
        .library(
            name: "WippleParser",
            targets: ["WippleParser"]
        ),
    ],
    dependencies: [
        .package(
            url: "https://github.com/nerdsupremacist/Syntax.git",
            from: "1.0.0"
        ),
        .package(
            name: "Wipple",
            path: "../Language"
        ),
    ],
    targets: [
        .target(
            name: "WippleParser",
            dependencies: ["Syntax", "Wipple"],
            path: "Sources"
        ),
    ]
)
