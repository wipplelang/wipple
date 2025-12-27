package statements

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type ExpressionStatementNode struct {
	Expression database.Node
	Facts      *database.Facts
}

func (node *ExpressionStatementNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseExpressionStatement(parser *syntax.Parser, parseExpression ParseExpressionFunc) (*ExpressionStatementNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := ParseComments(parser)
	if err != nil {
		return nil, err
	}

	expression, err := parseExpression(parser)
	if err != nil {
		return nil, err
	}

	return &ExpressionStatementNode{
		Expression: expression,
		Facts:      database.NewFacts(span()),
	}, nil
}

func (node *ExpressionStatementNode) Visit(visitor *visit.Visitor) {
	visitor.AfterAllDefinitions(func() {
		visitor.Visit(node.Expression)
		visitor.Db.Graph.Edge(node.Expression, node, "expression")
		visitor.Constraint(typecheck.NewGroupConstraint(node, node.Expression))
	})
}

func (node *ExpressionStatementNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	if err := c.Write(node.Expression); err != nil {
		return err
	}

	c.WriteString(span, ";")
	c.WriteLine()

	return nil
}
