package expressions

import (
	"slices"

	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/patterns"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type FunctionExpressionNode struct {
	Inputs []database.Node
	Output database.Node
	Facts  *database.Facts

	inputTemporaries []database.Node
}

func (node *FunctionExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseFunctionExpression(parser *syntax.Parser) (*FunctionExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	inputs, err := ParseFunctionExpressionInputs(parser)
	if err != nil {
		return nil, err
	}

	output, err := ParseExpression(parser)
	if err != nil {
		return nil, err
	}

	return &FunctionExpressionNode{
		Inputs: inputs,
		Output: output,
		Facts:  database.NewFacts(span()),
	}, nil
}

func ParseFunctionExpressionInputs(parser *syntax.Parser) ([]database.Node, *syntax.Error) {
	many, err := syntax.ParseMany(parser, 1, patterns.ParseAtomicPattern, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("FunctionOperator", syntax.TokenConfig{
		Commit: "in this function",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	inputs := make([]database.Node, 0, len(many))
	for _, item := range many {
		inputs = append(inputs, item.Value)
	}

	return inputs, nil
}

func (node *FunctionExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.PushScope()

	node.inputTemporaries = make([]database.Node, 0, len(node.Inputs))
	for _, pattern := range node.Inputs {
		node.inputTemporaries = append(node.inputTemporaries, visitor.VisitMatching(pattern))
	}

	visitor.Visit(node.Output)

	visitor.PopScope()

	visitor.Constraint(typecheck.TypeConstraint(node, typecheck.FunctionType(node.Inputs, node.Output)))
}

func (node *FunctionExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.inputTemporaries == nil {
		return c.Error(node)
	}

	span := database.GetSpanFact(node)

	c.WriteString(span, "(async (")
	for _, temporary := range node.inputTemporaries {
		c.WriteNode(span, temporary)
		c.WriteString(span, ", ")
	}
	c.WriteString(span, ") => {")
	c.WriteLine()

	for _, temporary := range patterns.CollectTemporaries(node.Inputs...) {
		if slices.Contains(node.inputTemporaries, temporary) {
			continue
		}

		c.WriteString(span, "var ")
		c.WriteNode(span, temporary)
		c.WriteString(span, ";")
		c.WriteLine()
	}

	for _, pattern := range node.Inputs {
		c.WriteString(span, "if (true")
		if err := c.Write(pattern); err != nil {
			return err
		}
		c.WriteString(span, `) {} else { throw new Error("unreachable"); }`)
		c.WriteLine()
	}

	c.WriteString(span, "return ")
	if err := c.Write(node.Output); err != nil {
		return err
	}
	c.WriteString(span, ";")
	c.WriteLine()
	c.WriteString(span, "})")

	return nil
}
