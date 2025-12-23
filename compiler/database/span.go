package database

import (
	"fmt"
	"math"
	"slices"
	"strings"
)

type Span struct {
	Path   string   `json:"path"`
	Start  Location `json:"start"`
	End    Location `json:"end"`
	Source string   `json:"source"`
}

type Location struct {
	Line   int `json:"line"`
	Column int `json:"column"`
	Index  int `json:"index"`
}

func JoinSpans(left Span, right Span, source string) Span {
	return Span{
		Path:   left.Path,
		Start:  left.Start,
		End:    right.End,
		Source: source[left.Start.Index:max(right.End.Index, left.Start.Index)],
	}
}

func CompareSpans(left Span, right Span) int {
	if left.Path != right.Path {
		return strings.Compare(left.Path, right.Path)
	}

	if left.Start.Index != right.Start.Index {
		return left.Start.Index - right.Start.Index
	}

	if left.End.Index != right.End.Index {
		return left.End.Index - right.End.Index
	}

	return 0
}

func SpansAreEqual(left Span, right Span) bool {
	return CompareSpans(left, right) == 0
}

func HaveEqualSpans(left Node, right Node) bool {
	return SpansAreEqual(GetSpanFact(left), GetSpanFact(right))
}

func (span Span) String() string {
	return fmt.Sprintf("%s:%d:%d", span.Path, span.Start.Line, span.Start.Column)
}

func NullSpan() Span {
	return Span{
		Path:   "",
		Start:  NullLocation(),
		End:    NullLocation(),
		Source: "",
	}
}

func NullLocation() Location {
	return Location{
		Line:   1,
		Column: 1,
		Index:  0,
	}
}

func SortByProximity(nodes []Node, source Node) {
	sourceSpan := GetSpanFact(source)
	slices.SortStableFunc(nodes, func(left Node, right Node) int {
		leftSpan := GetSpanFact(left)
		rightSpan := GetSpanFact(right)

		// Deprioritize nodes located before `source`
		if leftSpan.Start.Index < sourceSpan.Start.Index {
			return math.MaxInt32
		}
		if rightSpan.Start.Index < sourceSpan.Start.Index {
			return math.MinInt32
		}

		leftDistance := sourceSpan.Start.Index - leftSpan.Start.Index
		leftDistance = max(leftDistance, -leftDistance)

		rightDistance := sourceSpan.Start.Index - rightSpan.Start.Index
		rightDistance = max(rightDistance, -rightDistance)

		return leftDistance - rightDistance
	})
}
