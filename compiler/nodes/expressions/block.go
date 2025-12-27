package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/statements"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type BlockExpressionNode struct {
	Statements []database.Node
	Facts      *database.Facts
}

func (node *BlockExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseBlockExpression(parser *syntax.Parser) (*BlockExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftBrace")
	if err != nil {
		return nil, err
	}

	statementNodes, err := statements.ParseStatements(parser, ParseExpression)
	if err != nil {
		return nil, err
	}

	_, err = statements.ParseComments(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("RightBrace")
	if err != nil {
		return nil, err
	}

	return &BlockExpressionNode{
		Statements: statementNodes,
		Facts:      database.NewFacts(span()),
	}, nil
}

func (node *BlockExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.PushScope()

	for _, statement := range node.Statements {
		visitor.Visit(statement)
		visitor.Db.Graph.Edge(statement, node, "statement")
	}

	visitor.PopScope()

	var output typecheck.Type
	if len(node.Statements) > 0 {
		output = node.Statements[len(node.Statements)-1]
	} else {
		output = typecheck.TupleType([]typecheck.Type{})
	}

	visitor.Constraint(typecheck.NewTypeConstraint(node, typecheck.BlockType(output)))
}

func (node *BlockExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	c.WriteString(span, "(async () => {")
	c.WriteLine()

	for i, statement := range node.Statements {
		if i == len(node.Statements)-1 {
			c.WriteString(span, "return ")
		}

		if err := c.Write(statement); err != nil {
			return err
		}

		c.WriteString(span, ";")
		c.WriteLine()
	}

	c.WriteString(span, "})")

	return nil
}
