import SwiftUI
import UniformTypeIdentifiers
import NanoID

extension UTType {
    static var wipplePlayground: UTType {
        UTType(importedAs: "org.wipple.playground")
    }
}

struct PlaygroundDocument: FileDocument {
    var json: String

    init(json: String) {
        self.json = json
    }
    
    init() {
        self.json = """
        {
            "owner": "user",
            "collaborators": [],
            "name": "Untitled",
            "lastModified": "\(Date().formatted(.iso8601))",
            "pages": [
                {
                    "id": "\(ID().generate(size: 20))",
                    "name": "Untitled",
                    "items": []
                }
            ]
        }
        """
    }

    static var readableContentTypes: [UTType] { [.wipplePlayground] }

    init(configuration: ReadConfiguration) throws {
        guard let data = configuration.file.regularFileContents,
              let json = String(data: data, encoding: .utf8)
        else {
            throw CocoaError(.fileReadCorruptFile)
        }
        
        self.init(json: json)
    }
    
    func fileWrapper(configuration: WriteConfiguration) throws -> FileWrapper {
        FileWrapper(regularFileWithContents: self.json.data(using: .utf8)!)
    }
}
