import SwiftUI

@main
struct App: SwiftUI.App {
    var body: some Scene {
        #if compiler(>=6.0)
        if #available(iOS 18.0, *) {
            WelcomeScene()
        }
        #endif
        
        DocumentGroup(newDocument: PlaygroundDocument()) { file in
            EditorView(document: file.$document)
                .ignoresSafeArea(.container)
        }
    }
}
