package test

import (
	"testing"

	"wipple/database"
	"wipple/nodes/expressions"
	"wipple/nodes/statements"
	"wipple/syntax"
)

func parseStatement(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return statements.ParseStatement(parser, expressions.ParseExpression)
}

func TestParseTypeDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "-- Documentation comment\n[foo]\nFoo : type")
}

func TestParseGenericTypeDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "Foo : value => type")
}

func TestParseMarkerTypeDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "Foo : type")
}

func TestParseStructureTypeDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, `Foo : type {
    a :: A
    b :: B
    }`)
}

func TestParseEnumerationTypeDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, `Foo : type {
    Some Number
    None
    }`)
}

func TestParseTraitDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "Foo : trait Number")
}

func TestParseGenericTraitDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "Foo : value => trait (value -> Number)")
}

func TestParseConstantDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "show :: value -> Unit where (Show value)")
}

func TestParseSimpleValuedInstanceDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "instance (Foo Number) : 3.14")
}

func TestParseComplexValuedInstanceDefinition(t *testing.T) {
	syntax.TestParse(t, parseStatement, "instance (Foo (Maybe value)) where (Foo value) : 3.14")
}

func TestParseAssignment(t *testing.T) {
	syntax.TestParse(t, parseStatement, "x : 123")
}

func TestParseExpressionStatement(t *testing.T) {
	syntax.TestParse(t, parseStatement, "123")
}
