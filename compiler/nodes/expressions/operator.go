package expressions

import (
	"fmt"
	"slices"

	"wipple/codegen"
	"wipple/database"
	"wipple/nodes/statements"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type OperatorExpressionNode struct {
	Operator     string
	OperatorSpan database.Span
	Left         database.Node
	Right        database.Node
	Facts        *database.Facts

	operatorNode database.Node
}

func (node *OperatorExpressionNode) GetFacts() *database.Facts {
	return node.Facts
}

type Associativity int

const (
	LeftAssociative Associativity = iota
	RightAssociative
)

func parseOperator(operators []string, associativity Associativity, parseElement syntax.ParseFunc[database.Node]) syntax.ParseFunc[database.Node] {
	return func(parser *syntax.Parser) (database.Node, *syntax.Error) {
		elements, err := syntax.ParseMany(parser, 1, parseElement, func(parser *syntax.Parser) (string, *syntax.Error) {
			parser.ConsumeLineBreaks()

			var operator string
			for _, op := range operators {
				operatorString, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (string, *syntax.Error) {
					return parser.Token(op)
				})

				if err != nil {
					return "", err
				}

				if ok {
					operator = operatorString
					break
				}
			}

			if operator == "" {
				return "", parser.Error("Expected operator")
			}

			parser.ConsumeLineBreaks()

			return operator, nil
		})
		if err != nil {
			return nil, err
		}

		first := elements[0]
		rest := elements[1:]

		result := first.Value
		switch associativity {
		case LeftAssociative:
			for _, element := range rest {
				operator := element.Separator
				operatorSpan := element.SeparatorSpan
				right := element.Value

				span := database.JoinSpans(database.GetSpanFact(result), database.GetSpanFact(right), parser.Source)

				result = &OperatorExpressionNode{
					Operator:     operator,
					OperatorSpan: operatorSpan,
					Left:         result,
					Right:        right,
					Facts:        database.NewFacts(span),
				}
			}
		case RightAssociative:
			for _, element := range slices.Backward(rest) {
				operator := element.Separator
				operatorSpan := element.SeparatorSpan
				right := element.Value

				span := database.JoinSpans(database.GetSpanFact(result), database.GetSpanFact(right), parser.Source)

				result = &OperatorExpressionNode{
					Operator:     operator,
					OperatorSpan: operatorSpan,
					Left:         result,
					Right:        right,
					Facts:        database.NewFacts(span),
				}
			}
		}

		return result, nil
	}
}

func ParseOperatorExpression(parser *syntax.Parser) (database.Node, *syntax.Error) {
	// From highest precedence to lowest precedence
	var (
		parseToExpression       = parseOperator([]string{"ToOperator"}, RightAssociative, ParseExpressionElement)
		parseByExpression       = parseOperator([]string{"ByOperator"}, LeftAssociative, parseToExpression)
		parsePowerExpression    = parseOperator([]string{"PowerOperator"}, RightAssociative, parseByExpression)
		parseMultiplyExpression = parseOperator([]string{"MultiplyOperator", "DivideOperator", "RemainderOperator"}, LeftAssociative, parsePowerExpression)
		parseAddExpression      = parseOperator([]string{"AddOperator", "SubtractOperator"}, LeftAssociative, parseMultiplyExpression)
		parseCompareExpression  = parseOperator([]string{"LessThanOrEqualOperator", "LessThanOperator", "GreaterThanOrEqualOperator", "GreaterThanOperator"}, LeftAssociative, parseAddExpression)
		parseEqualExpression    = parseOperator([]string{"EqualOperator", "NotEqualOperator"}, LeftAssociative, parseCompareExpression)
		parseAndExpression      = parseOperator([]string{"AndOperator"}, LeftAssociative, parseEqualExpression)
		parseOrExpression       = parseOperator([]string{"OrOperator"}, LeftAssociative, parseAndExpression)
		parseApplyExpression    = parseOperator([]string{"ApplyOperator"}, LeftAssociative, parseOrExpression)
	)

	return parseApplyExpression(parser)
}

func (node *OperatorExpressionNode) Visit(visitor *visit.Visitor) {
	visitExpression(visitor, node)

	visitor.Visit(node.Left)
	visitor.Visit(node.Right)

	visitOperator, ok := operators[node.Operator]
	if !ok {
		panic(fmt.Sprintf("unknown operator: %s", node.Operator))
	}

	node.operatorNode = visitOperator(visitor, node.OperatorSpan, node, node.Left, node.Right)
}

func (node *OperatorExpressionNode) Codegen(c *codegen.Codegen) error {
	if node.operatorNode == nil {
		return c.Error(node)
	}

	return c.Write(node.operatorNode)
}

func resolveOperatorTrait(visitor *visit.Visitor, operatorSpan database.Span, node database.Node, name string) database.Node {
	operatorNode := &ConstructorExpressionNode{
		ConstructorName: name,
		Facts:           database.NewFacts(operatorSpan),
	}
	visitor.Visit(operatorNode)
	return operatorNode
}

type visitOperator func(visitor *visit.Visitor, operatorSpan database.Span, node database.Node, left database.Node, right database.Node) database.Node

func traitOperator(trait string) visitOperator {
	return func(visitor *visit.Visitor, operatorSpan database.Span, node database.Node, left database.Node, right database.Node) database.Node {
		operatorNode := resolveOperatorTrait(visitor, operatorSpan, node, trait)

		visitor.Constraint(typecheck.NewTypeConstraint(operatorNode, typecheck.FunctionType([]database.Node{left, right}, node)))

		return &CallExpressionNode{
			Function: operatorNode,
			Inputs:   []database.Node{left, right},
			Facts:    database.NewFacts(database.GetSpanFact(node)),
		}
	}
}

func shortCircuitOperator(trait string) visitOperator {
	return func(visitor *visit.Visitor, operatorSpan database.Span, node database.Node, left database.Node, right database.Node) database.Node {
		operatorNode := resolveOperatorTrait(visitor, operatorSpan, node, trait)

		visitor.Constraint(typecheck.NewTypeConstraint(operatorNode, typecheck.FunctionType[typecheck.Type]([]typecheck.Type{left, typecheck.BlockType(right)}, node)))

		return &CallExpressionNode{
			Function: operatorNode,
			Inputs: []database.Node{
				left,
				&BlockExpressionNode{
					Statements: []database.Node{
						&statements.ExpressionStatementNode{
							Expression: right,
							Facts:      database.NewFacts(database.GetSpanFact(right)),
						},
					},
					Facts: database.NewFacts(database.GetSpanFact(right)),
				},
			},
			Facts: database.NewFacts(database.GetSpanFact(node)),
		}
	}
}

func applyOperator(visitor *visit.Visitor, operatorSpan database.Span, node database.Node, left database.Node, right database.Node) database.Node {
	visitor.Constraint(typecheck.NewTypeConstraint(right, typecheck.FunctionType([]database.Node{left}, node)))

	return &CallExpressionNode{
		Function: right,
		Inputs:   []database.Node{left},
		Facts:    database.NewFacts(operatorSpan),
	}
}

var operators = map[string]visitOperator{
	"to":  traitOperator("To"),
	"by":  traitOperator("By"),
	"^":   traitOperator("Power"),
	"*":   traitOperator("Multiply"),
	"/":   traitOperator("Divide"),
	"%":   traitOperator("Remainder"),
	"+":   traitOperator("Add"),
	"-":   traitOperator("Subtract"),
	"<":   traitOperator("Less-Than"),
	"<=":  traitOperator("Less-Than-Or-Equal"),
	">":   traitOperator("Greater-Than"),
	">=":  traitOperator("Greater-Than-Or-Equal"),
	"=":   traitOperator("Equal"),
	"/=":  traitOperator("Not-Equal"),
	"and": shortCircuitOperator("And"),
	"or":  shortCircuitOperator("Or"),
	".":   applyOperator,
}
