package test

import (
	"testing"

	"wipple/nodes/expressions"
	"wipple/syntax"
)

func TestParseVariableExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "foo")
}

func TestParseNumberExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "3.14")
}

func TestParseStringExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, `"abc"`)
}

func TestParseFormatExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, `"Hello, _!" name`)
}

func TestParseStructureExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, `Foo {
    a : b
    c : d
    }`)
}

func TestParseEmptyBlockExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseBlockExpression, "{}")
}

func TestParseBlockExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "{foo}")
}

func TestParseDoExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "do foo")
}

func TestParseSimpleIntrinsicExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, `intrinsic "message"`)
}

func TestParseComplexIntrinsicExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, `intrinsic "message" x y`)
}

func TestParseWhenExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, `when x {
    a -> b
    c -> d
    }`)
}

func TestParseCallExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "f x y")
}

func TestParseAnnotateExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "(3.14 :: Number)")
}

// func TestParseSimpleApplyExpression(t *testing.T) {
// 	syntax.TestParse(t, expressions.ParseExpression, "x . f")
// }

// func TestParseComplexApplyExpression(t *testing.T) {
// 	syntax.TestParse(t, expressions.ParseExpression, "a b . c d")
// }

func TestParseAsExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "x as T")
}

// func TestParseAddExpression(t *testing.T) {
// 	syntax.TestParse(t, expressions.ParseExpression, "a + b")
// }

func TestParseEmptyCollectionExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "(,)")
}

func TestParseSingleElementCollectionExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "a,")
}

func TestParseSingleLineCollectionExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "a, b, c")
}

func TestParseMultilineCollectionExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, `(
    a,
    b,
    c,
    )`)
}

func TestParseSingleInputFunctionExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "x -> y")
}

func TestParseMultiInputFunctionExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "x y -> z")
}

func TestParseComplexInputFunctionExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "(X y) -> z")
}

func TestParseSimpleIsExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "x is None")
}

func TestParseComplexIsExpression(t *testing.T) {
	syntax.TestParse(t, expressions.ParseExpression, "x is Some 3.14")
}
