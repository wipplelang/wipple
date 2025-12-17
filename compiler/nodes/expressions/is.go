package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/patterns"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type IsExpressionNode struct {
	Left  database.Node
	Right database.Node
	Facts *database.Facts

	inputTemporary database.Node
	trueVariant    database.Node
	falseVariant   database.Node
}

func (node *IsExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseIsExpression(parser *syntax.Parser) (*IsExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	left, err := ParseExpressionElement(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("IsOperator")
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	right, err := patterns.ParsePatternElement(parser)
	if err != nil {
		return nil, err
	}

	return &IsExpressionNode{
		Left:  left,
		Right: right,
		Facts: database.NewFacts(span()),
	}, nil
}

func (node *IsExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.Visit(node.Left)

	node.inputTemporary = &database.HiddenNode{Facts: database.NewFacts(database.GetSpanFact(node.Left))}
	visitor.Db.Register(node.inputTemporary)
	visit.Matching(visitor, node.inputTemporary, false, func() struct{} {
		visitor.Visit(node.Right)
		return struct{}{}
	})

	visitor.Constraint(typecheck.GroupConstraint(node.inputTemporary, node.Left))

	booleanTypeDefinition, ok := visit.Resolve[*visit.TypeDefinition](visitor, "Boolean", node)
	if !ok {
		return
	}

	trueVariant, ok := visit.Resolve[*visit.VariantConstructorDefinition](visitor, "True", node)
	if !ok {
		return
	}

	falseVariant, ok := visit.Resolve[*visit.VariantConstructorDefinition](visitor, "False", node)
	if !ok {
		return
	}

	node.trueVariant = trueVariant.Node
	node.falseVariant = falseVariant.Node

	visitor.Constraint(typecheck.GroupConstraint(node, booleanTypeDefinition.Node))
}

func (node *IsExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.inputTemporary == nil || node.trueVariant == nil || node.falseVariant == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	whenExpression := &WhenExpressionNode{
		Input: node.Left,
		Arms: []Arm{
			{
				Pattern: node.Right,
				Value:   node.trueVariant,
				Span:    span,
			},
			{
				Pattern: &patterns.WildcardPatternNode{
					Facts: database.NewFacts(span),
				},
				Value: node.falseVariant,
				Span:  span,
			},
		},
		Facts:          database.NewFacts(span),
		inputTemporary: node.inputTemporary,
	}

	return c.Write(whenExpression)
}
