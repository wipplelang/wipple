#if swift(>=6.0)
import SwiftUI

@available(iOS 18.0, *)
struct WelcomeScene: Scene {
    @State private var showLessons = false
    @State private var onSelectLesson: CheckedContinuation<PlaygroundDocument?, any Error>?
    
    var body: some Scene {
        DocumentGroupLaunchScene("Wipple") {
            NewDocumentButton("New Playground")
            
            NewDocumentButton("Browse Lessons", for: PlaygroundDocument.self) {
               try await withCheckedThrowingContinuation { continuation in
                   self.onSelectLesson = continuation
                   self.showLessons = true
               }
            }
            .popover(isPresented: self.$showLessons) {
                LessonPicker { lesson in
                    self.onSelectLesson?.resume(returning: lesson)
                    self.onSelectLesson = nil
                }
                .presentationSizing(.form)
            }
        } background: {
            ZStack {
                Color(uiColor: .secondarySystemGroupedBackground)
                
                Image("Background")
                    .resizable()
                    .scaledToFill()
            }
            .ignoresSafeArea()
        }
    }
}
#endif
