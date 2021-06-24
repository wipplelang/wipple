// swift-tools-version:5.4

import PackageDescription

let package = Package(
    name: "WippleProjects",
    platforms: [.macOS("10.11")],
    products: [
        .library(
            name: "WippleProjects",
            targets: ["WippleProjects"]
        ),
    ],
    dependencies: [
        .package(
            name: "WippleStdlib",
            path: "../../Library"
        ),
        .package(
            url: "https://github.com/weichsel/ZIPFoundation",
            .upToNextMinor(from: "0.9.0")
        ),
    ],
    targets: [
        .target(
            name: "WippleProjects",
            dependencies: ["WippleStdlib", "ZIPFoundation"],
            path: "Sources"
        ),
    ]
)
