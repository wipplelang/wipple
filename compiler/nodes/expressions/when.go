package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/patterns"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type WhenExpressionNode struct {
	Input database.Node
	Arms  []Arm
	Facts *database.Facts

	inputTemporary database.Node
}

func (node *WhenExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

type Arm struct {
	Pattern database.Node
	Value   database.Node
	Span    database.Span
}

func ParseWhenExpression(parser *syntax.Parser) (*WhenExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("WhenKeyword")
	if err != nil {
		return nil, err
	}

	parser.Commit("in this `when` expression")

	input, err := ParseAtomicExpression(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("LeftBrace")
	if err != nil {
		return nil, err
	}

	arms, err := ParseArms(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBrace")
	if err != nil {
		return nil, err
	}

	return &WhenExpressionNode{
		Input: input,
		Arms:  arms,
		Facts: database.NewFacts(span()),
	}, nil
}

func ParseArm(parser *syntax.Parser) (Arm, *syntax.Error) {
	span := parser.Spanned()

	pattern, err := patterns.ParseAtomicPattern(parser)
	if err != nil {
		return Arm{}, err
	}

	parser.Commit("in this `when` arm")

	_, err = parser.Token("FunctionOperator")
	if err != nil {
		return Arm{}, err
	}

	parser.ConsumeLineBreaks()

	value, err := ParseExpression(parser)
	if err != nil {
		return Arm{}, err
	}

	return Arm{
		Pattern: pattern,
		Value:   value,
		Span:    span(),
	}, nil
}

func ParseArms(parser *syntax.Parser) ([]Arm, *syntax.Error) {
	return syntax.ParseLines(parser, 0, true, ParseArm)
}

func (node *WhenExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.Visit(node.Input)
	visitor.Db.Graph.Edge(node.Input, node, "input")

	node.inputTemporary = &database.HiddenNode{
		Facts: database.NewFacts(database.GetSpanFact(node.Input)),
	}

	visitor.Db.Register(node.inputTemporary)

	visitor.Constraint(typecheck.NewGroupConstraint(node.inputTemporary, node.Input))

	visit.Matching(visitor, node.inputTemporary, true, func() struct{} {
		for _, arm := range node.Arms {
			visitor.PushScope()
			visitor.Visit(arm.Pattern)
			visitor.Db.Graph.Edge(arm.Pattern, node, "pattern")
			visitor.Visit(arm.Value)
			visitor.Db.Graph.Edge(arm.Value, node, "value")
			visitor.PopScope()

			visitor.Constraint(typecheck.NewGroupConstraint(arm.Value, node))
		}

		return struct{}{}
	})
}

func (node *WhenExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.inputTemporary == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	c.WriteString(span, "await (async (")
	c.WriteNode(span, node.inputTemporary)
	c.WriteString(span, ") => {")
	c.WriteLine()

	armPatterns := make([]database.Node, 0, len(node.Arms))
	for _, arm := range node.Arms {
		armPatterns = append(armPatterns, arm.Pattern)
	}

	for _, temporary := range patterns.CollectTemporaries(armPatterns...) {
		if temporary == node.inputTemporary {
			continue
		}

		c.WriteString(span, "var ")
		c.WriteNode(span, temporary)
		c.WriteString(span, ";")
		c.WriteLine()
	}

	for _, arm := range node.Arms {
		c.WriteString(span, "if (true")
		if err := c.Write(arm.Pattern); err != nil {
			return err
		}
		c.WriteString(span, ") {")
		c.WriteLine()
		c.WriteString(span, "return ")
		if err := c.Write(arm.Value); err != nil {
			return err
		}
		c.WriteString(span, ";")
		c.WriteLine()
		c.WriteString(span, "}")
		c.WriteLine()
	}

	c.WriteString(span, `throw new Error("unreachable");`)
	c.WriteLine()

	c.WriteString(span, "})(")
	if err := c.Write(node.Input); err != nil {
		return err
	}
	c.WriteString(span, ")")

	return nil
}
