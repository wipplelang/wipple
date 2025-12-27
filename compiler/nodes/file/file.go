package file

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/expressions"
	"wipple/nodes/statements"
	"wipple/syntax"
	"wipple/visit"
)

type FileNode struct {
	Statements []database.Node
	Facts      *database.Facts
}

func (node *FileNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseFile(parser *syntax.Parser) (*FileNode, *syntax.Error) {
	span := parser.Spanned()

	stmts, err := statements.ParseStatements(parser, expressions.ParseExpression)
	if err != nil {
		return nil, err
	}

	_, err = statements.ParseComments(parser)
	if err != nil {
		return nil, err
	}

	return &FileNode{
		Statements: stmts,
		Facts:      database.NewFacts(span()),
	}, nil
}

func (node *FileNode) Visit(visitor *visit.Visitor) {
	for _, statement := range node.Statements {
		visitor.Visit(statement)
		visitor.Db.Graph.Edge(statement, node, "statement")
	}
}

func (node *FileNode) Codegen(c *codegen.Codegen) error {
	for _, statement := range node.Statements {
		if err := c.Write(statement); err != nil {
			return err
		}
	}

	return nil
}
