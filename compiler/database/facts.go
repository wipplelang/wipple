package database

import (
	"fmt"
	"maps"
	"reflect"
	"slices"
	"strings"
)

type Facts map[reflect.Type]any

func EmptyFacts() *Facts {
	return &Facts{}
}

func NewFacts(span Span) *Facts {
	facts := EmptyFacts()
	(*facts)[reflect.TypeFor[SpanFact]()] = SpanFact(span)
	return facts
}

func GetFact[T any](node Node) (T, bool) {
	fact, ok := (*node.GetFacts())[reflect.TypeFor[T]()].(T)
	return fact, ok
}

func SetFact[T any](node Node, fact T) {
	(*node.GetFacts())[reflect.TypeFor[T]()] = fact
}

func CloneFacts(facts *Facts) *Facts {
	cloned := maps.Clone(*facts)
	return &cloned
}

type SpanFact Span

func (fact SpanFact) String() string {
	return fmt.Sprintf("at %v", Span(fact))
}

func SetSpanFact(node Node, span Span) {
	SetFact(node, SpanFact(span))
}

func GetSpanFact(node Node) Span {
	span, ok := GetFact[SpanFact](node)
	if !ok {
		return NullSpan()
	}

	return Span(span)
}

func (facts *Facts) String() string {
	s := ""

	keys := make([]reflect.Type, 0, len(*facts))
	for key := range *facts {
		keys = append(keys, key)
	}

	slices.SortFunc(keys, func(a, b reflect.Type) int {
		return strings.Compare(a.String(), b.String())
	})

	for _, key := range keys {
		value := (*facts)[key]

		if _, ok := value.(fmt.Stringer); ok {
			s += fmt.Sprintf("  %v\n", value)
		} else {
			s += fmt.Sprintf("  %s(%v)\n", reflect.TypeOf(value).Name(), value)
		}
	}

	if s == "" {
		s = "  (no facts)"
	}

	return s
}
