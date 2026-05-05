import Compiler
import JavaScriptKit

extension CompileResult {
    @JS func executable() -> String? {
        do {
            let program = try codegen(
                db: self.db,
                files: self.files,
                libraryFiles: self.libFiles ?? [],
            )

            return try program.wasm(db: self.db)
        } catch { return nil }
    }
}
