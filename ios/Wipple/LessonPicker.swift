import SwiftUI
import SwiftyJSON

struct LessonPicker: View {
    let onSelectLesson: (PlaygroundDocument?) -> Void
    
    @State private var lessons: [(String, Lesson)] = []
    
    var body: some View {
        NavigationStack {
            ScrollView {
                ForEach(self.lessons, id: \.1.id) { json, lesson in
                    Button {
                        let lessonJSON = JSON(parseJSON: json)
                        
                        let playgroundJSON: JSON = [
                            "owner": "user",
                            "collaborators": [],
                            "name": lessonJSON["name"],
                            "lastModified": Date().formatted(.iso8601),
                            "pages": lessonJSON["pages"],
                            "locked": true,
                        ]
                        
                        self.onSelectLesson(PlaygroundDocument(json: playgroundJSON.rawString(.utf8)!))
                    } label: {
                        HStack {
                            VStack(alignment: .leading, spacing: 4) {
                                Text(lesson.name)
                                    .font(.headline.weight(.semibold))
                                
                                Text(lesson.description)
                                    .font(.callout)
                            }
                            .multilineTextAlignment(.leading)
                            
                            Spacer()
                            
                            Image(systemName: "chevron.forward")
                        }
                        .padding()
                        .background(Color.accentColor.opacity(0.1))
                        .clipShape(RoundedRectangle(cornerRadius: 10))
                    }
                }
            }
            .contentMargins(20, for: .scrollContent)
            .navigationTitle("Lessons")
            .navigationBarTitleDisplayMode(.large)
        }
        .onAppear {
            let indexData = try! Data(contentsOf: Bundle.main.url(forResource: "dist/lessons/index", withExtension: "json")!)
            let index = try! JSONDecoder().decode([String].self, from: indexData)
            
            let lessons = index.map { id in
                let lessonData = try! Data(contentsOf: Bundle.main.url(forResource: "dist/lessons/\(id)", withExtension: "json")!)
                
                return (
                    String(data: lessonData, encoding: .utf8)!,
                    try! JSONDecoder().decode(Lesson.self, from: lessonData)
                )
            }
            
            self.lessons = lessons
        }
    }
}

private struct Lesson: Codable, Identifiable {
    var id: String
    var name: String
    var description: String
}
