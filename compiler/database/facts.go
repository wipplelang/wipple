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

type ParentFact struct {
	Parent Node
}

func (fact ParentFact) String() string {
	return ""
}

func SetParentFact(node Node, parent Node) {
	SetFact(node, ParentFact{Parent: parent})
}

func GetParentFact(node Node) Node {
	fact, ok := GetFact[ParentFact](node)
	if !ok {
		return nil
	}

	return fact.Parent
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

		if stringer, ok := value.(fmt.Stringer); ok {
			valueString := stringer.String()
			if valueString != "" {
				s += fmt.Sprintf("  %v\n", value)
			}
		} else {
			s += fmt.Sprintf("  %s(%v)\n", reflect.TypeOf(value).Name(), value)
		}
	}

	if s == "" {
		s = "  (no facts)"
	}

	return s
}
