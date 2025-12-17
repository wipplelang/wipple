package test

import (
	"testing"

	"wipple/nodes/constraints"
	"wipple/syntax"
)

func TestParseBoundConstraint(t *testing.T) {
	syntax.TestParse(t, constraints.ParseConstraint, `(Foo value)`)
}

func TestParseDefaultConstraint(t *testing.T) {
	syntax.TestParse(t, constraints.ParseConstraint, `(value :: Number)`)
}
