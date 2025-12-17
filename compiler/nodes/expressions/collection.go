package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type CollectionExpressionNode struct {
	Elements []database.Node
	Facts    *database.Facts

	collectionNode database.Node
}

func (node *CollectionExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseEmptyCollectionExpression(parser *syntax.Parser) (*CollectionExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("CollectionOperator")
	if err != nil {
		return nil, err
	}

	return &CollectionExpressionNode{
		Elements: nil,
		Facts:    database.NewFacts(span()),
	}, nil
}

func ParseCollectionExpression(parser *syntax.Parser) (*CollectionExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	many, err := syntax.ParseMany(parser, 1, ParseExpressionElement, func(parser *syntax.Parser) (*struct{}, *syntax.Error) {
		_, err := parser.Token("CollectionOperator")
		if err != nil {
			return nil, err
		}

		parser.ConsumeLineBreaks()

		return &struct{}{}, nil
	})
	if err != nil {
		return nil, err
	}

	if len(many) == 1 {
		_, err = parser.Token("CollectionOperator")
		if err != nil {
			return nil, err
		}
	} else {
		_, _, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (struct{}, *syntax.Error) {
			_, err := parser.Token("CollectionOperator")
			if err != nil {
				return struct{}{}, err
			}

			return struct{}{}, nil
		})
		if err != nil {
			return nil, err
		}
	}

	elements := make([]database.Node, 0, len(many))
	for _, item := range many {
		elements = append(elements, item.Value)
	}

	return &CollectionExpressionNode{
		Elements: elements,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *CollectionExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	elementType := &database.HiddenNode{
		Facts: database.NewFacts(database.GetSpanFact(node)),
	}
	visitor.Db.Register(elementType)
	database.SetFact(elementType, typecheck.TypedFact{})

	if len(node.Elements) > 0 {
		visitor.Constraint(typecheck.GroupConstraint(node.Elements[0], elementType))
	}

	node.collectionNode = database.Node(&ConstructorExpressionNode{
		ConstructorName: "Initial-Collection",
		Facts:           database.NewFacts(database.GetSpanFact(node)),
	})

	for _, element := range node.Elements {
		node.collectionNode = &CallExpressionNode{
			Function: &ConstructorExpressionNode{
				ConstructorName: "Build-Collection",
				Facts:           database.NewFacts(database.GetSpanFact(element)),
			},
			Inputs: []database.Node{element, node.collectionNode},
			Facts:  database.NewFacts(database.GetSpanFact(element)),
		}
	}

	visitor.Visit(node.collectionNode)
	visitor.Constraint(typecheck.GroupConstraint(node.collectionNode, node))
}

func (node *CollectionExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.collectionNode == nil {
		return c.Error(node)
	}

	return c.Write(node.collectionNode)
}
