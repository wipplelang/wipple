// swift-tools-version:5.4

import PackageDescription

let package = Package(
    name: "WipplePlaygroundInterpreter",
    platforms: [.macOS("10.15.4")],
    products: [
        .executable(
            name: "WipplePlaygroundInterpreter",
            targets: ["WipplePlaygroundInterpreter"]
        ),
    ],
    dependencies: [
        .package(
            name: "WippleStdlib",
            path: "../../Library"
        ),
    ],
    targets: [
        .executableTarget(
            name: "WipplePlaygroundInterpreter",
            dependencies: ["WippleStdlib"],
            path: "Sources"
        ),
    ]
)
