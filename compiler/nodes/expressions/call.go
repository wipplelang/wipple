package expressions

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type CallExpressionNode struct {
	Function database.Node
	Inputs   []database.Node
	Facts    *database.Facts

	isUnit bool
}

func (node *CallExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseCallExpression(parser *syntax.Parser) (*CallExpressionNode, *syntax.Error) {
	span := parser.Spanned()

	functionValue, err := ParseAtomicExpression(parser)
	if err != nil {
		return nil, err
	}

	many, err := syntax.ParseMany(parser, 1, ParseAtomicExpression, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	inputs := make([]database.Node, 0, len(many))
	for _, item := range many {
		inputs = append(inputs, item.Value)
	}

	return &CallExpressionNode{
		Function: functionValue,
		Inputs:   inputs,
		Facts:    database.NewFacts(span()),
	}, nil
}

func (node *CallExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	if len(node.Inputs) == 1 {
		if unit, ok := node.Inputs[0].(*VariableExpressionNode); ok {
			if definitions, ok := visit.Peek[*visit.ConstantDefinition](visitor, unit.Variable); ok {
				unitDefinition := definitions[0]

				if unitDefinition.Attributes.Unit {
					visitor.Visit(unit)
					visitor.Visit(node.Function)

					visitor.Constraint(typecheck.NewTypeConstraint(unit, typecheck.FunctionType[database.Node]([]database.Node{node.Function}, node)))

					node.isUnit = true

					return
				}
			}
		}
	}

	for _, input := range node.Inputs {
		visitor.Visit(input)
	}

	visitor.Visit(node.Function)

	visitor.Constraint(typecheck.NewTypeConstraint(node.Function, typecheck.FunctionType[database.Node](node.Inputs, node)))
}

func (node *CallExpressionNode) Codegen(c *codegen.Codegen) error {
	span := database.GetSpanFact(node)

	if node.isUnit {
		c.WriteString(span, "await (")
		if err := c.Write(node.Inputs[0]); err != nil {
			return err
		}
		c.WriteString(span, ")(")
		if err := c.Write(node.Function); err != nil {
			return err
		}
		c.WriteString(span, ")")

	} else {
		c.WriteString(span, "await (")
		if err := c.Write(node.Function); err != nil {
			return err
		}
		c.WriteString(span, ")(")

		for _, input := range node.Inputs {
			if err := c.Write(input); err != nil {
				return err
			}

			c.WriteString(span, ", ")
		}

		c.WriteString(span, ")")
	}

	return nil
}
