package statements

import (
	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/patterns"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type AssignmentNode struct {
	Pattern database.Node
	Value   database.Node
	Facts   *database.Facts

	temporary database.Node
}

func (node *AssignmentNode) GetFacts() *database.Facts {
	return node.Facts
}

func ParseAssignmentStatement(parser *syntax.Parser, parseExpression ParseExpressionFunc) (*AssignmentNode, *syntax.Error) {
	span := parser.Spanned()

	_, err := ParseComments(parser)
	if err != nil {
		return nil, err
	}

	pattern, err := patterns.ParsePattern(parser)
	if err != nil {
		return nil, err
	}

	_, err = parser.Token("AssignOperator")
	if err != nil {
		return nil, err
	}

	parser.Commit("in this variable assignment")

	parser.ConsumeLineBreaks()

	value, err := parseExpression(parser)
	if err != nil {
		return nil, err
	}

	return &AssignmentNode{
		Pattern: pattern,
		Value:   value,
		Facts:   database.NewFacts(span()),
	}, nil
}

func (node *AssignmentNode) Visit(visitor *visit.Visitor) {
	visitor.AfterAllDefinitions(func() {
		// Try assigning to an existing constant if possible
		if pattern, ok := node.Pattern.(*patterns.VariablePatternNode); ok {
			if definitions, ok := visit.Peek[*visit.ConstantDefinition](visitor, pattern.Variable); ok {
				definition := definitions[len(definitions)-1]
				if !definition.Assigned {
					constantType := definition.Value

					visit.Defining(visitor, definition.Node, func() (*visit.ConstantDefinition, bool) {
						visitor.CurrentDefinition.WithinConstantValue = true

						visitor.Visit(node.Value)

						visitor.Constraint(typecheck.NewGroupConstraint(node.Value, constantType))

						definition.Assigned = true
						definition.Value = node.Value

						return nil, false
					})

					return
				}
			}
		}

		visitor.Visit(node.Value)

		node.temporary = &database.HiddenNode{
			Facts: database.NewFacts(database.GetSpanFact(node)),
		}

		visitor.Db.Register(node.temporary)

		visit.Matching(visitor, node.temporary, true, func() struct{} {
			visitor.Visit(node.Pattern)
			return struct{}{}
		})

		visitor.Constraint(typecheck.NewGroupConstraint(node.Pattern, node.Value))
	})
}

func (node *AssignmentNode) Codegen(c *codegen.Codegen) error {
	if node.temporary == nil {
		return nil // assigned to constant
	}

	span := database.GetSpanFact(node)

	c.WriteString(span, "var ")
	c.WriteNode(span, node.temporary)
	c.WriteString(span, ";")
	c.WriteLine()

	for _, temporary := range patterns.CollectTemporaries(node.Pattern) {
		if temporary == node.temporary {
			continue
		}

		c.WriteString(span, "var ")
		c.WriteNode(span, temporary)
		c.WriteString(span, ";")
		c.WriteLine()
	}

	c.WriteNode(span, node.temporary)
	c.WriteString(span, " = ")
	if err := c.Write(node.Value); err != nil {
		return err
	}
	c.WriteString(span, ";")
	c.WriteLine()

	c.WriteString(span, "if (true")
	if err := c.Write(node.Pattern); err != nil {
		return err
	}
	c.WriteString(span, `) {} else { throw new Error("unreachable"); }`)
	c.WriteLine()

	return nil
}
