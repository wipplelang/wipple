// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "Wipple",
    platforms: [
        .iOS("13.4"),
        .macOS("10.15.4"),
    ],
    products: [
        .executable(
            name: "wipple-interpreter",
            targets: ["Wipple"]
        ),
        .library(
            name: "Wipple",
            targets: ["WippleLib"]
        ),
    ],
    targets: [
        .target(name: "WippleLib"),
        .target(
            name: "Wipple",
            dependencies: ["WippleLib"]
        ),
        .testTarget(
            name: "WippleTests",
            dependencies: ["WippleLib"]
        ),
    ]
)
