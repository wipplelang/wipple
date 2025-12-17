package patterns

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type InvalidSetPatternFact struct{}

func (fact InvalidSetPatternFact) String() string {
	return "is invalid `set` pattern"
}

type SetPatternNode struct {
	Variable string
	Facts    *database.Facts

	matching database.Node

	matchingVariable database.Node
}

func (node *SetPatternNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseSetPattern(parser *syntax.Parser) (*SetPatternNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("SetKeyword", syntax.TokenConfig{
		Commit: "in this `set` pattern",
	})
	if err != nil {
		return nil, err
	}

	name, err := syntax.ParseVariableName(parser)
	if err != nil {
		return nil, err
	}

	return &SetPatternNode{
		Variable: name,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *SetPatternNode) Visit(visitor *visit.Visitor) {
	visitPattern(visitor, node)
	node.matching = visitor.CurrentMatch.Node

	variableDefinition, ok := visit.Resolve[*visit.VariableDefinition](visitor, node.Variable, node)
	if !ok {
		return
	}

	visitor.Constraint(typecheck.NewGroupConstraint(node, variableDefinition.Node))

	node.matchingVariable = variableDefinition.Node

	if !visitor.CurrentMatch.AllowSet {
		database.SetFact(node, InvalidSetPatternFact{})
	}
}

func (node *SetPatternNode) Codegen(c *codegen.Codegen) error {
	if node.matchingVariable == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	c.WriteString(span, " && ((")
	c.WriteNode(span, node.matchingVariable)
	c.WriteString(span, " = ")
	c.WriteNode(span, node.matching)
	c.WriteString(span, ") || true)")

	return nil
}

func (node *SetPatternNode) EachTemporary(f func(database.Node)) {
	// Do NOT yield `node.matchingVariable`, that would shadow it!
}
