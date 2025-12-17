package test

import (
	"testing"

	"wipple/nodes/types"
	"wipple/syntax"
)

func TestParsePlaceholderType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "_")
}

func TestParseUnitType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "()")
}

func TestParseSimpleNamedType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "Number")
}

func TestParseComplexNamedType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "Maybe Number")
}

func TestParseBlockType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "{Number}")
}

func TestParseSingleInputFunctionType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "Number -> ()")
}

func TestParseMultiInputFunctionType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "Number Number -> ()")
}

func TestParseComplexInputFunctionType(t *testing.T) {
	syntax.TestParse(t, types.ParseType, "(Maybe Number) Number -> ()")
}
