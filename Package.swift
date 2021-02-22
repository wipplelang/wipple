// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "Wipple",
    products: [
        .library(name: "Wipple", targets: ["Wipple"])
    ],
    targets: [
        .target(
            name: "Wipple",
            path: "Sources"
        )
    ]
)
