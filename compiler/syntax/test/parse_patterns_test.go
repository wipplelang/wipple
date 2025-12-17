package test

import (
	"testing"

	"wipple/nodes/patterns"
	"wipple/syntax"
)

func TestParseWildcardPattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "_")
}

func TestParseVariablePattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "x")
}

func TestParseStructurePattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "Foo {x : y}")
}

func TestParseSetPattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "set x")
}

func TestParseSimpleConstructorPattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "None")
}

func TestParseComplexConstructorPattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "Some x y z")
}

func TestParseSimpleOrPattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "x or y")
}

func TestParseComplexOrPattern(t *testing.T) {
	syntax.TestParse(t, patterns.ParsePattern, "x or y or z")
}
