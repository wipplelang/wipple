import Foundation
import Syntax
import Wipple

public enum Token: Hashable {
    case shebang
    case name(String)
    case number(Decimal)
    case text(String)
    case quote
    case backslash
    case openParenthesis
    case closeParenthesis
    case openBracket
    case closeBracket
    case openBrace
    case closeBrace
    case whitespace
    case newline
    case comment(String)
}

public func lex(_ code: String, file: SourceFile? = nil) -> [TokenWithLocation] {
    // The lexer should never fail because all characters are covered
    try! Lexer(file: file).parse(code, options: [])
}

struct Lexer: Parser {
    let file: SourceFile?

    var body: AnyParser<[TokenWithLocation]> {
        // Shebang
        RegularExpression(#"#!(.*)"#)
            .maybe()
            .ignoreOutput()

        Repeat {
            Either {
                RegularExpression(#"--.*"#)
                    .map { Token.comment(String($0.text)) }

                StringLiteral()
                    .map(Token.text)

                RegularExpression(#"-?[0-9]+(?:\.[0-9]+)?"#)
                    .map { Token.number(Decimal(string: String($0.text))!) }

                RegularExpression(#"[^ \t\n\(\)\[\]{}'\\"]+"#)
                    .map { Token.name(String($0.text)) }

                "'".map(to: Token.quote)
                "\\".map(to: Token.backslash)
                "(".map(to: Token.openParenthesis)
                ")".map(to: Token.closeParenthesis)
                "[".map(to: Token.openBracket)
                "]".map(to: Token.closeBracket)
                "{".map(to: Token.openBrace)
                "}".map(to: Token.closeBrace)

                RegularExpression(#"[ \t]+"#)
                    .map(to: Token.whitespace)

                RegularExpression(#"\n"#)
                    .map(to: Token.newline)
            }
            .withLocation(file: self.file)
        }
    }
}

// FIXME: Workaround until tuples conform to protocols
public struct TokenWithLocation: Hashable {
    let token: Token
    let location: SourceLocation

    public init(token: Token, location: SourceLocation) {
        self.token = token
        self.location = location
    }

    var tuple: (Token, SourceLocation) {
        (self.token, self.location)
    }
}

extension Parser where Output == Token {
    func withLocation(file: SourceFile?) -> AnyParser<TokenWithLocation> {
        mapWithLocation { output, location in
            TokenWithLocation(
                token: output,
                location: SourceLocation(
                    file: file,
                    line: location.lowerBound.line + 1,
                    column: location.lowerBound.column + 1
                )
            )
        }
    }
}
