package syntax

import (
	"reflect"
	"testing"

	"wipple/database"

	"github.com/gkampitakis/go-snaps/snaps"
)

type SyntaxErrorNode struct {
	Facts *database.Facts
}

func (node *SyntaxErrorNode) GetFacts() *database.Facts {
	return node.Facts
}

func Parse[T database.Node](db *database.Db, path string, source string, f ParseFunc[T]) (T, *Error) {
	var result T

	parser, err := NewParser(db, path, source)
	if err == nil {
		result, err = f(parser)
	}
	if err == nil {
		err = parser.Finish()
	}

	if err != nil {
		node := &SyntaxErrorNode{
			Facts: database.NewFacts(err.Span),
		}

		db.Register(node)

		SetSyntaxErrorFact(node, *err)

		return result, err
	}

	return result, nil
}

func TestParse[T database.Node](t *testing.T, f ParseFunc[T], source string) {
	db := database.NewDb(nil)

	result, err := Parse(db, "test", source, f)
	if err != nil {
		panic(err)
	}

	var removeFacts func(value reflect.Value)
	removeFacts = func(value reflect.Value) {
		switch value.Kind() {
		case reflect.Pointer, reflect.Interface:
			if !value.IsNil() {
				removeFacts(value.Elem())
			}
		case reflect.Slice:
			for i := 0; i < value.Len(); i++ {
				removeFacts(value.Index(i))
			}
		case reflect.Struct:
			for i := 0; i < value.NumField(); i++ {
				field := value.Field(i)

				if field.Type() == reflect.TypeFor[*database.Facts]() || field.Type() == reflect.TypeFor[database.Span]() {
					field.Set(reflect.Zero(field.Type()))
				} else {
					removeFacts(field)
				}
			}
		case reflect.Map:
			for _, key := range value.MapKeys() {
				removeFacts(value.MapIndex(key))
			}
		}
	}

	removeFacts(reflect.ValueOf(result))

	snaps.MatchSnapshot(t, result)
}
