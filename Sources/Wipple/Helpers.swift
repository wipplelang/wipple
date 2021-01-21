import Foundation

@discardableResult
func shell(_ command: String, _ args: String...) -> String {
    let task = Process()
    let pipe = Pipe()

    task.standardOutput = pipe
    task.launchPath = command
    task.arguments = args
    task.launch()

    let data = pipe.fileHandleForReading.readDataToEndOfFile()
    let output = String(data: data, encoding: .utf8)!

    return output
}
