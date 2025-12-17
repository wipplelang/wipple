package syntax

import (
	"strings"
	"wipple/database"

	lex "github.com/timtadh/lexmachine"
	"github.com/timtadh/lexmachine/machines"
)

type Token struct {
	kind  string
	value string
	span  database.Span
}

type tokenRule struct {
	kind          string
	pattern       string
	name          string
	skip          bool
	defaultReason string
	trim          func(s string) string
}

var rules = []tokenRule{
	{kind: "Space", pattern: `[ \t]+`, name: "", skip: true},
	{kind: "LineBreak", pattern: `\n+`, name: "a line break"},
	{kind: "Comment", pattern: `--[^\n]*`, name: "a comment", trim: func(s string) string { return s[2:] }},
	{kind: "TypeFunctionOperator", pattern: `=>`, name: "`=>`"},
	{kind: "AnnotateOperator", pattern: `::`, name: "`::`"},
	{kind: "AssignOperator", pattern: `:`, name: "`:`"},
	{kind: "FunctionOperator", pattern: `->`, name: "`->`"},
	{kind: "LessThanOrEqualOperator", pattern: `<=`, name: "`<=`"},
	{kind: "GreaterThanOrEqualOperator", pattern: `>=`, name: "`>=`"},
	{kind: "NotEqualOperator", pattern: `/=`, name: "`/=`"},
	{kind: "PowerOperator", pattern: `\^`, name: "`^`"},
	{kind: "MultiplyOperator", pattern: `\*`, name: "`*`"},
	{kind: "DivideOperator", pattern: `/`, name: "`/`"},
	{kind: "RemainderOperator", pattern: `%`, name: "`%`"},
	{kind: "AddOperator", pattern: `\+`, name: "`+`"},
	{kind: "SubtractOperator", pattern: `-`, name: "`-`"},
	{kind: "LessThanOperator", pattern: `<`, name: "`<`"},
	{kind: "GreaterThanOperator", pattern: `>`, name: "`>`"},
	{kind: "EqualOperator", pattern: `=`, name: "`=`"},
	{kind: "ApplyOperator", pattern: `\.`, name: "`.`"},
	{kind: "TupleOperator", pattern: `;`, name: "`;`"},
	{kind: "CollectionOperator", pattern: `,`, name: "`,`"},
	{kind: "LeftParenthesis", pattern: `\(`, name: "`(`"},
	{kind: "RightParenthesis", pattern: `\)`, name: "`)`"},
	{kind: "LeftBracket", pattern: `\[`, name: "`[`"},
	{kind: "RightBracket", pattern: `\]`, name: "`]`"},
	{kind: "LeftBrace", pattern: `\{`, name: "`{`"},
	{kind: "RightBrace", pattern: `\}`, name: "`}`"},
	{kind: "UnderscoreKeyword", pattern: `_`, name: "`_`"},
	{kind: "DoKeyword", pattern: `do`, name: "`do`"},
	{kind: "InferKeyword", pattern: `infer`, name: "`infer`"},
	{kind: "InstanceKeyword", pattern: `instance`, name: "`instance`"},
	{kind: "IntrinsicKeyword", pattern: `intrinsic`, name: "`intrinsic`"},
	{kind: "SetKeyword", pattern: `set`, name: "`set`"},
	{kind: "TraitKeyword", pattern: `trait`, name: "`trait`"},
	{kind: "TypeKeyword", pattern: `type`, name: "`type`"},
	{kind: "WhenKeyword", pattern: `when`, name: "`when`"},
	{kind: "WhereKeyword", pattern: `where`, name: "`where`"},
	{kind: "AsOperator", pattern: `as`, name: "`as`"},
	{kind: "ToOperator", pattern: `to`, name: "`to`"},
	{kind: "ByOperator", pattern: `by`, name: "`by`"},
	{kind: "IsOperator", pattern: `is`, name: "`is`"},
	{kind: "AndOperator", pattern: `and`, name: "`and`"},
	{kind: "OrOperator", pattern: `or`, name: "`or`"},
	{kind: "Number", pattern: `[+\-]?\d+(\.\d+)?`, name: "a number"},
	{kind: "String", pattern: `"[^"]*"|'[^']*'`, name: "a string", trim: func(s string) string { return s[1 : len(s)-1] }},
	{kind: "CapitalName", pattern: `(\d+-)*[A-Z][A-Za-z0-9_]*(-[A-Za-z0-9_]+)*([!?])?`, name: "a capital name"},
	{kind: "LowercaseName", pattern: `(\d+-)*[A-Za-z0-9_]+(-[A-Za-z0-9_]+)*([!?])?`, name: "a lowercase name"},
}

var lexer *lex.Lexer

var tokenIds = make(map[string]int, len(rules))
var tokenKinds = make([]string, 0, len(rules))

func token(name string, trim func(s string) string) lex.Action {
	return func(s *lex.Scanner, m *machines.Match) (any, error) {
		if _, ok := tokenIds[name]; !ok {
			tokenIds[name] = len(tokenIds)
			tokenKinds = append(tokenKinds, name)
		}

		tokenString := string(m.Bytes)
		if trim != nil {
			tokenString = trim(tokenString)
		}

		return s.Token(tokenIds[name], tokenString, m), nil
	}
}

func skip(*lex.Scanner, *machines.Match) (any, error) {
	return nil, nil
}

var tokenNames = make(map[string]string, len(rules))
var defaultTokenReasons = make(map[string]string, len(rules))

func init() {
	lexer = lex.NewLexer()

	for _, rule := range rules {
		f := skip
		if !rule.skip {
			f = token(rule.kind, rule.trim)
		}

		lexer.Add([]byte(rule.pattern), f)
		tokenNames[rule.kind] = rule.name
		defaultTokenReasons[rule.kind] = rule.defaultReason
	}

	err := lexer.CompileNFA()
	if err != nil {
		panic(err)
	}
}

func Tokenize(path string, source string) ([]*Token, *Error) {
	scanner, err := lexer.Scanner([]byte(source))
	if err != nil {
		panic(err)
	}

	var tokens []*Token
	for token, err, eof := scanner.Next(); !eof; token, err, eof = scanner.Next() {
		token := token.(*lex.Token)
		startIndex := token.TC
		endIndex := scanner.TC

		start := database.Location{
			Index:  startIndex,
			Line:   token.StartLine,
			Column: token.StartColumn,
		}

		end := database.Location{
			Index:  endIndex,
			Line:   token.EndLine,
			Column: token.EndColumn,
		}

		span := database.Span{
			Path:   path,
			Start:  start,
			End:    end,
			Source: source[startIndex:endIndex],
		}

		if err != nil {
			return nil, &Error{
				Message: "Unexpected character",
				Span:    span,
			}
		}

		tokens = append(tokens, &Token{
			kind:  tokenKinds[token.Type],
			value: token.Value.(string),
			span:  span,
		})
	}

	return tokens, nil
}

func TokenIsKeyword(kind string) bool {
	return strings.HasSuffix(kind, "Keyword")
}

func TokenIsOperator(kind string) bool {
	return strings.HasSuffix(kind, "Operator") || kind == "WhereKeyword"
}

func TokenIsBinaryOperator(kind string) bool {
	return TokenIsOperator(kind) && !TokenIsNonAssociativeOperator(kind) && !TokenIsVariadicOperator(kind)
}

func TokenIsNonAssociativeOperator(kind string) bool {
	return kind == "WhereKeyword" || kind == "TypeFunctionOperator" || kind == "AnnotateOperator" || kind == "AssignOperator"
}

func TokenIsVariadicOperator(kind string) bool {
	return kind == "TupleOperator" || kind == "CollectionOperator"
}

func TokenIsOpening(kind string) bool {
	return strings.HasPrefix(kind, "Left")
}

func TokenIsClosing(kind string) bool {
	return strings.HasPrefix(kind, "Right")
}
