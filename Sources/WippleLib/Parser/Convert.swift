import Foundation

public extension AST {
    func convertToValue() -> Value {
        if let block = self.block {
            let statements = block.map { statement in
                statement.map { $0.convertToValue() }
            }

            return Value(location: self.location)
                .add(.block(statements))
        } else if let list = self.list {
            let items = list.map { $0.convertToValue() }

            return Value(location: self.location)
                .add(.list(items))
        } else if let name = self.name {
            return Value(location: self.location)
                .add(.name(name))
        } else if let text = self.text {
            return Value(location: self.location)
                .add(.text(text))
        } else if let number = self.number {
            let number = Decimal(string: number)!

            return Value(location: self.location)
                .add(.number(number))
        } else if let quoted = self.quoted?.value {
            let quotedValue = quoted.convertToValue()

            return Value(location: self.location)
                .add(.quoted(quotedValue))
        } else {
            fatalError("Invalid input")
        }
    }
}
