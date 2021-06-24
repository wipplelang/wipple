import Wipple
import WippleParser

public extension AST {
    var value: Value {
        var value: Value

        switch self.node {
        case .block(let statements):
            value = Value(Block(rawValue: statements.map { statement in
                List(rawValue: statement.items.map(\.value))
            }))
        case .list(let items):
            value = Value(List(rawValue: items.map(\.value)))
        case .literal(let node):
            value = Value(Literal(rawValue: node.value))
        case .escaped(let node):
            value = Value(Escaped(rawValue: node.value))
        case .name(let name):
            value = Value(Name(rawValue: name))
        case .number(let number):
            value = Value(Number(rawValue: number))
        case .text(let text):
            value = Value(Text(rawValue: text))
        }

        value.addLocation(self.location)

        return value
    }
}
