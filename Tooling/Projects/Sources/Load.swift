import Wipple
import WippleStdlib

/// Import a file/folder using a module name
public func importModule(name: String, _ stack: Stack) throws -> Module {
    try importPath(resolveModule(name: name, stack), stack)
}



/// Import a file, returning a module. If this file belongs to a project, the
/// file's environment will descend from the project's environment.
public func importFile(_ file: SourceFile, _ stack: Stack) throws -> Module {
    try importFile(file, in: stack.projectEnv, stack)
}
