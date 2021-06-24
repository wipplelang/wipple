import Foundation
import Wipple
import WippleStdlib
import ZIPFoundation

public extension Env {
    private static var projectEnvs: [URL: Env] = [:]

    static func forProject(withPath path: URL) -> Env {
        projectEnvs[path] ?? {
            let env = Env.global.child()
            projectEnvs[path] = env
            return env
        }()
    }
}

public extension Stack {
    private enum ProjectRootKey: StackKey {
        typealias Value = URL
    }

    var projectRoot: URL? {
        get { self[ProjectRootKey.self] }
        set { self[ProjectRootKey.self] = newValue }
    }

    var projectEnv: Env {
        guard let projectRoot = self.projectRoot else {
            return .global
        }

        return .forProject(withPath: projectRoot)
    }
}

public extension Stack {
    private enum DependencyPathKey: StackKey {
        typealias Value = URL
    }

    var dependencyPath: URL? {
        get { self[DependencyPathKey.self] }
        set { self[DependencyPathKey.self] = newValue }
    }

    func dependencyPath(_ stack: Stack) throws -> URL {
        guard let dependencyPath = self.dependencyPath else {
            throw Exit.error("Dependency path not set", stack)
        }

        return dependencyPath
    }
}

public struct Project {
    public var path: URL
    public var dependencies: [String: Dependency]
    public var mainModuleName: String
}

public extension Project {
    init(file: SourceFile, _ stack: Stack) throws {
        var stack = stack
        stack.diagnostics.add(file.description.map { "Loading project '\($0)'" } ?? "Loading project")

        let env = Env.forProject(withPath: file.path).child()
        try setupProjectFile(path: file.path.deletingLastPathComponent(), env, stack)

        let module = try importFile(file, in: env, stack)

        self = try parseProjectFile(file, module.capturedEnv, stack)
    }
}

public extension Project {
    enum Dependency: Hashable, Primitive {
        case local(path: URL)
        case remote(url: URL)
        case git(repo: String, branch: String?)

        public var isLocal: Bool {
            if case .local = self {
                return true
            } else {
                return false
            }
        }

        public func update(installPath: URL, onInstall: () -> Void, _ stack: Stack) throws -> Project {
            func projectFile(in path: URL) throws -> Project {
                try Project(file: .init(path: path.appendingPathComponent("project.wpl")), stack)
            }

            let path = self.cachePath(for: installPath)

            if !self.isLocal && FileManager.default.fileExists(atPath: path.path) {
                return try projectFile(in: path)
            }

            onInstall()

            do {
                switch self {
                case .local(let dependencyPath):
                    try FileManager.default.copyItem(at: dependencyPath, to: path)
                case .remote(let url):
                    try Self.downloadZipFile(url: url, extractingTo: path)
                case .git(let repo, let branch):
                    let success = try Self.downloadGitRepo(repo, branch: branch, to: path)

                    guard success else {
                        struct GitError: Swift.Error, CustomStringConvertible {
                            let description = "Error downloading Git repository"
                        }

                        throw GitError()
                    }
                }
            } catch {
                throw Exit.error("Failed to install dependency: \(error)", stack)
            }

            return try projectFile(in: path)
        }

        public func cachePath(for path: URL) -> URL {
            path.appendingPathComponent(String(path.hashValue, radix: 16, uppercase: false))
        }

        private static func downloadZipFile(url: URL, extractingTo path: URL) throws {
            let group = DispatchGroup()
            var result: Result<Void, Swift.Error>!

            let task = URLSession.shared.downloadTask(with: url) { downloadURL, _, error in
                if let error = error {
                    result = .failure(error)
                    return
                }

                result = Result {
                    try FileManager.default.unzipItem(at: downloadURL!, to: path)
                }
            }

            group.enter()
            task.resume()
            group.wait()

            try result.get()
        }

        private static func downloadGitRepo(_ repo: String, branch: String?, to path: URL) throws -> Bool {
            let process = Process()
            process.launchPath = "/usr/bin/env"

            process.arguments = ["git", "clone", repo, "--quiet"]
            if let branch = branch {
                process.arguments!.append(contentsOf: ["--branch", branch])
            }
            process.arguments!.append(path.path)

            process.launch()
            process.waitUntilExit()

            return process.terminationStatus == EXIT_SUCCESS
        }
    }
}

private func setupProjectFile(path projectRoot: URL, _ env: Env, _ stack: Stack) throws {
    try env.setVariable(
        "path",
        to: Value(Function { value, env, stack in
            let path = try value
                .evaluate(env, stack)
                .get(Text.self, or: "Expected text", env, stack)
                .rawValue

            let dependencyPath = projectRoot
                .appendingPathComponent(path)
                .standardizedFileURL

            return Value(Project.Dependency.local(path: dependencyPath))
        }),
        stack
    )

    try env.setVariable(
        "url",
        to: Value(Function { value, env, stack in
            let urlString = try value
                .evaluate(env, stack)
                .get(Text.self, or: "Expected URL", env, stack)
                .rawValue

            guard let url = URL(string: urlString) else {
                throw Exit.error("Expected URL", stack)
            }

            return Value(Project.Dependency.remote(url: url))
        }),
        stack
    )

    try env.setVariable(
        "git",
        to: Value(Function { value, env, stack in
            let repoString = try value
                .evaluate(env, stack)
                .get(Text.self, or: "Expected Git repository URL", env, stack)
                .rawValue

            let repo: String
            let branch: String?

            if repoString.contains(" ") {
                let components = repoString.split(separator: " ", omittingEmptySubsequences: false)

                guard components.count == 2 else {
                    throw Exit.error("Expected a URL and a branch, separated by a space", stack)
                }

                repo = String(components[0])
                branch = String(components[1])
            } else {
                repo = repoString
                branch = nil
            }

            return Value(Project.Dependency.git(repo: repo, branch: branch))
        }),
        stack
    )

    // TODO: Rename to 'platform'
    try env.setVariable("target", to: Value(Text(rawValue: platform)), stack)
}

private func parseProjectFile(_ file: SourceFile, _ env: Env, _ stack: Stack) throws -> Project {
    let dependencies: [String: Project.Dependency] = try {
        var stack = stack
        stack.diagnostics.add("Updating dependencies")

        guard let dependenciesValue = try Name(rawValue: "dependencies").resolveIfPresent(env, stack) else {
            return [:]
        }

        let dependenciesModule = try dependenciesValue
            .get(Module.self, or: "'dependencies' must be a module", env, stack)

        return try Dictionary(uniqueKeysWithValues: dependenciesModule.capturedEnv.variables.map { name, variable in
            var stack = stack
            stack.diagnostics.add("Parsing dependency '\(name)'")

            let dependency = try variable
                .getValue(env, stack)
                .get(Project.Dependency.self, or: "Expected dependency", env, stack)

            return (name, dependency)
        })
    }()

    let mainModuleName: String = try {
        var stack = stack
        stack.diagnostics.add(
            file.description
                .map { "Resolving main module in project '\($0)'" }
                ?? "Resolving main module in project"
        )

        return try Name(rawValue: "main")
            .resolve(env, stack)
            .get(Text.self, or: "Expected a path", env, stack)
            .rawValue
    }()

    return Project(
        path: file.path,
        dependencies: dependencies,
        mainModuleName: mainModuleName
    )
}
