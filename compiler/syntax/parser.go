package syntax

import (
	"fmt"
	"reflect"

	db "wipple/database"
)

type ParseFunc[T any] func(*Parser) (T, *Error)

type Parser struct {
	Db     *db.Db
	Path   string
	Source string
	tokens []*Token
	index  int
	stack  []*commitEntry
	cache  map[uintptr]map[int]parseResult
}

type commitEntry struct {
	trace string
}

type parseResult struct {
	index int
	value any
}

func NewParser(db *db.Db, path string, source string) (*Parser, *Error) {
	parser := &Parser{
		Db:     db,
		Path:   path,
		Source: source,
		cache:  map[uintptr]map[int]parseResult{},
	}

	var err *Error
	parser.tokens, err = Tokenize(path, source)
	if err != nil {
		return nil, err
	}

	return parser, nil
}

func (parser *Parser) Spanned() func() db.Span {
	index := parser.index

	return func() db.Span {
		start := parser.eofSpan()
		if index < len(parser.tokens) {
			start = parser.tokens[index].span
		}

		end := parser.eofSpan()
		if parser.index > 0 {
			end = parser.tokens[parser.index-1].span
		}

		return db.JoinSpans(start, end, parser.Source)
	}
}

func (parser *Parser) backtrack(index int) {
	parser.index = index
}

func (parser *Parser) Error(message string) *Error {
	return parser.ErrorWithReason(message, "")
}

func (parser *Parser) ErrorWithReason(message string, reason string) *Error {
	span := parser.eofSpan()
	if parser.index < len(parser.tokens) {
		span = parser.tokens[parser.index].span
	}

	return &Error{
		Message: message,
		Reason:  reason,
		Span:    span,
	}
}

type TokenConfig struct {
	Name   string
	Reason string
}

func (parser *Parser) Token(kind string, configs ...TokenConfig) (string, *Error) {
	var config TokenConfig
	if len(configs) > 0 {
		config = configs[0]
	}

	expected := config.Name
	if expected == "" {
		expected = tokenNames[kind]
	}

	reason := config.Reason
	if reason == "" {
		reason = defaultTokenReasons[kind]
	}

	if parser.index >= len(parser.tokens) {
		return "", &Error{
			Message: fmt.Sprintf("Expected %s", kind),
			Reason:  config.Reason,
			Span:    parser.eofSpan(),
		}
	}

	token := parser.tokens[parser.index]

	if token.kind != kind {
		return "", &Error{
			Message: fmt.Sprintf("Expected %s, but found %s", expected, tokenNames[token.kind]),
			Reason:  reason,
			Span:    token.span,
		}
	}

	parser.index += 1

	return token.value, nil
}

func (parser *Parser) Commit(trace string) {
	parser.stack[len(parser.stack)-1].trace = trace
}

func (parser *Parser) ConsumeLineBreaks() {
	parser.Token("LineBreak")
}

func (parser *Parser) eofSpan() db.Span {
	if len(parser.tokens) == 0 {
		return db.NullSpan()
	} else {
		return parser.tokens[len(parser.tokens)-1].span
	}
}

func (parser *Parser) Finish() *Error {
	if parser.index < len(parser.tokens) {
		token := parser.tokens[parser.index]
		return parser.Error(fmt.Sprintf("Unexpected %s", tokenNames[token.kind]))
	}

	return nil
}

func ParseNothing(parser *Parser) (struct{}, *Error) {
	return struct{}{}, nil
}

func ParseCached[T any](parser *Parser, f ParseFunc[T]) (T, *Error) {
	start := parser.index

	key := reflect.ValueOf(f).Pointer()
	entry, ok := parser.cache[key]
	if !ok {
		entry = map[int]parseResult{}
		parser.cache[key] = entry
	}

	cached, ok := entry[start]
	if ok {
		parser.index = cached.index
		return cached.value.(T), nil
	}

	result, err := f(parser)
	if err != nil {
		return result, err
	}

	entry[start] = parseResult{
		index: parser.index,
		value: result,
	}

	return result, nil
}

func ParseOptional[T any](parser *Parser, f ParseFunc[T]) (T, bool, *Error) {
	start := parser.index

	entry := &commitEntry{}
	parser.stack = append(parser.stack, entry)
	defer func() {
		parser.stack = parser.stack[:len(parser.stack)-1]
	}()

	var result T
	result, err := f(parser)
	if err != nil {
		if entry.trace != "" {
			err.Committed = entry.trace
		}

		if err.Committed != "" {
			return result, false, err
		}

		parser.backtrack(start)
		return result, false, nil
	}

	return result, true, nil
}

type Many[T any, S any] struct {
	Value     T
	Separator S
}

func ParseMany[T any, S any](parser *Parser, min int, f ParseFunc[T], separator ParseFunc[S]) ([]Many[T, S], *Error) {
	first := true

	var results []Many[T, S]
	for {
		start := parser.index

		var separatorResult S
		if !first {
			var ok bool
			var err *Error
			separatorResult, ok, err = ParseOptional(parser, separator)
			if err != nil {
				return nil, err
			}

			if !ok {
				break
			}
		}

		result, ok, err := ParseOptional(parser, f)
		if err != nil {
			return nil, err
		}

		if !ok {
			parser.backtrack(start)
			break
		}

		results = append(results, Many[T, S]{
			Value:     result,
			Separator: separatorResult,
		})

		first = false
	}

	if len(results) < min {
		return nil, &Error{
			Message: fmt.Sprintf("Expected at least %d items", min),
		}
	}

	return results, nil
}

func ParseLines[T any](parser *Parser, min int, require bool, f ParseFunc[T]) ([]T, *Error) {
	parser.ConsumeLineBreaks()

	result, err := ParseMany(parser, min, f, func(parser *Parser) (*struct{}, *Error) {
		if require {
			_, err := parser.Token("LineBreak")
			if err != nil {
				return nil, err
			}
		} else {
			parser.ConsumeLineBreaks()
		}

		return &struct{}{}, nil
	})

	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	values := make([]T, 0, len(result))
	for _, item := range result {
		values = append(values, item.Value)
	}

	return values, nil
}
