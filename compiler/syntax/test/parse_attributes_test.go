package test

import (
	"testing"

	"wipple/nodes/attributes"
	"wipple/syntax"
)

func TestParseNamedAttribute(t *testing.T) {
	syntax.TestParse(t, attributes.ParseAttribute, "[foo]")
}

func TestParseValuedAttribute(t *testing.T) {
	syntax.TestParse(t, attributes.ParseAttribute, `[a : "b"]`)
}
