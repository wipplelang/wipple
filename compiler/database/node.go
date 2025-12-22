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

func NodeSource(node Node) string {
	source := DefinitionSource(node)
	source = regexp.MustCompile(`(?s)\{.*\}`).ReplaceAllString(source, "{⋯}")   // collapse braces
	source = regexp.MustCompile(`(?s)::.*`).ReplaceAllString(source, "")        // remove type annotations
	source = regexp.MustCompile(`(?s)\bwhere\b.*`).ReplaceAllString(source, "") // remove bounds
	source = regexp.MustCompile(`(?s)\n.*`).ReplaceAllString(source, "⋯")       // collapse multiple lines

	return strings.TrimSpace(source)
}

func DefinitionSource(node Node) string {
	span := GetSpanFact(node)

	source := span.Source
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

func RenderNode(node Node) string {
	return renderSource(node, NodeSource(node))
}

func RenderDefinition(node Node) string {
	return renderSource(node, DefinitionSource(node))
}

func renderSource(node Node, source string) string {
	if LspEnabled {
		return colors.Code(source)
	} else {
		span := GetSpanFact(node)
		return fmt.Sprintf("%s %s", colors.Code(source), colors.Extra(span.String()))
	}
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
