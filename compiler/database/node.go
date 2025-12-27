package database

import (
	"fmt"
	"reflect"
	"regexp"
	"strings"

	"wipple/colors"
)

type Node interface {
	GetFacts() *Facts
}

type HiddenNode struct {
	Facts *Facts
}

func (node *HiddenNode) GetFacts() *Facts {
	return node.Facts
}

var hiddenNodes = map[reflect.Type]struct{}{
	reflect.TypeFor[*HiddenNode](): {},
}

func HideNode[T Node]() {
	hiddenNodes[reflect.TypeFor[T]()] = struct{}{}
}

func IsHiddenNode(node Node) bool {
	_, ok := hiddenNodes[reflect.TypeOf(node)]
	return ok
}

func DisplayNode(node Node) string {
	return fmt.Sprintf("%s %s", reflect.TypeOf(node).Elem().Name(), RenderNode(node))
}

func NodeSource(source string) string {
	source = DefinitionSource(source)
	source = regexp.MustCompile(`(?s)\{.*\}`).ReplaceAllString(source, "{⋯}") // collapse braces

	leftParenthesisIndex := strings.Index(source, "(")
	annotateIndex := strings.Index(source, "::")
	if annotateIndex != -1 && annotateIndex < leftParenthesisIndex {
		source = regexp.MustCompile(`(?s)::?.*`).ReplaceAllString(source, "") // remove assignments and type annotations
	}

	source = regexp.MustCompile(`(?s)\bwhere\b.*`).ReplaceAllString(source, "") // remove bounds
	source = regexp.MustCompile(`(?s)\n.*`).ReplaceAllString(source, "⋯")       // collapse multiple lines

	return strings.TrimSpace(source)
}

func DefinitionSource(source string) string {
	source = regexp.MustCompile(`(\[.*\]\s*)*`).ReplaceAllString(source, "") // strip attributes
	source = regexp.MustCompile(`--.*\n`).ReplaceAllString(source, "")       // strip comments

	// Remove parentheses
	if source != "()" && strings.HasPrefix(source, "(") && strings.HasSuffix(source, ")") {
		leftCount := 0
		for i := 0; i < len(source); i++ {
			if source[i] == '(' {
				leftCount++
			} else {
				break
			}
		}

		rightCount := 0
		for i := len(source) - 1; i >= 0; i-- {
			if source[i] == ')' {
				rightCount++
			} else {
				break
			}
		}

		trimCount := min(leftCount, rightCount)
		source = source[trimCount : len(source)-trimCount]
	}

	// Remove assigned value...
	index := strings.Index(source, ":")
	if index >= 0 {
		// ...but not type annotations or type/trait definitions
		rest := source[index+1:]
		if strings.HasPrefix(rest, ":") || strings.Contains(rest, " type ") || strings.Contains(rest, " trait ") {
			index = -1
		}
	}
	if index >= 0 {
		source = source[:index]
	}

	return strings.TrimSpace(source)
}

var WrapDisplayNode func(Node, string) string

func WrappingDisplayNode[T any](wrap func(Node, string) string, f func() T) T {
	prev := WrapDisplayNode
	WrapDisplayNode = wrap
	result := f()
	WrapDisplayNode = prev
	return result
}

func RenderNode(node Node) string {
	return RenderSource(node, NodeSource(GetSpanFact(node).Source))
}

func RenderDefinition(node Node) string {
	return RenderSource(node, DefinitionSource(GetSpanFact(node).Source))
}

func RenderSource(node Node, source string) string {
	if node != nil {
		if WrapDisplayNode != nil {
			return WrapDisplayNode(node, source)
		} else if !LspEnabled {
			span := GetSpanFact(node)
			return fmt.Sprintf("%s %s", colors.Code(source), colors.Extra(span.String()))
		}
	}

	return colors.Code(source)
}

type FilterFunc func(node Node) bool

func PathFilter(path string) FilterFunc {
	return func(node Node) bool {
		span := GetSpanFact(node)
		return span.Path == path
	}
}

func RangeFilter(path string, start int, end int) FilterFunc {
	return func(node Node) bool {
		span := GetSpanFact(node)
		return span.Path == path && span.Start.Index >= start && span.End.Index <= end
	}
}

func LineFilter(path string, line int) FilterFunc {
	return func(node Node) bool {
		span := GetSpanFact(node)
		return span.Path == path && span.Start.Line == line
	}
}
