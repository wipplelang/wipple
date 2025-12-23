package expressions

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type IsExpressionFact struct{}

func (fact IsExpressionFact) String() string {
	return "is an expression"
}

func ParseExpression(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		functionExpression, ok, err := syntax.ParseOptional(parser, ParseFunctionExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return functionExpression, nil
		}

		tupleExpression, ok, err := syntax.ParseOptional(parser, ParseTupleExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return tupleExpression, nil
		}

		emptyCollectionExpression, ok, err := syntax.ParseOptional(parser, ParseEmptyCollectionExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return emptyCollectionExpression, nil
		}

		collectionExpression, ok, err := syntax.ParseOptional(parser, ParseCollectionExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return collectionExpression, nil
		}

		isExpression, ok, err := syntax.ParseOptional(parser, ParseIsExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return isExpression, nil
		}

		asExpression, ok, err := syntax.ParseOptional(parser, ParseAsExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return asExpression, nil
		}

		annotateExpression, ok, err := syntax.ParseOptional(parser, ParseAnnotateExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return annotateExpression, nil
		}

		operatorExpression, ok, err := syntax.ParseOptional(parser, ParseOperatorExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return operatorExpression, nil
		}

		expressionElement, ok, err := syntax.ParseOptional(parser, ParseExpressionElement)
		if err != nil {
			return nil, err
		}
		if ok {
			return expressionElement, nil
		}

		return nil, parser.Error("Expected expression")
	})
}

func ParseExpressionElement(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		formatExpression, ok, err := syntax.ParseOptional(parser, ParseFormatExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return formatExpression, nil
		}

		structureExpression, ok, err := syntax.ParseOptional(parser, ParseStructureExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return structureExpression, nil
		}

		callExpression, ok, err := syntax.ParseOptional(parser, ParseCallExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return callExpression, nil
		}

		doExpression, ok, err := syntax.ParseOptional(parser, ParseDoExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return doExpression, nil
		}

		whenExpression, ok, err := syntax.ParseOptional(parser, ParseWhenExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return whenExpression, nil
		}

		intrinsicExpression, ok, err := syntax.ParseOptional(parser, ParseIntrinsicExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return intrinsicExpression, nil
		}

		atomicExpression, ok, err := syntax.ParseOptional(parser, ParseAtomicExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return atomicExpression, nil
		}

		return nil, parser.Error("Expected expression")
	})
}

func ParseAtomicExpression(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		placeholderExpression, ok, err := syntax.ParseOptional(parser, ParsePlaceholderExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return placeholderExpression, nil
		}

		variableExpression, ok, err := syntax.ParseOptional(parser, ParseVariableExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return variableExpression, nil
		}

		constructorExpression, ok, err := syntax.ParseOptional(parser, ParseConstructorExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return constructorExpression, nil
		}

		numberExpression, ok, err := syntax.ParseOptional(parser, ParseNumberExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return numberExpression, nil
		}

		stringExpression, ok, err := syntax.ParseOptional(parser, ParseStringExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return stringExpression, nil
		}

		blockExpression, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (*BlockExpressionNode, *syntax.Error) {
			return ParseBlockExpression(parser)
		})
		if err != nil {
			return nil, err
		}
		if ok {
			return blockExpression, nil
		}

		unitExpression, ok, err := syntax.ParseOptional(parser, ParseUnitExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return unitExpression, nil
		}

		parenthesizedExpression, ok, err := syntax.ParseOptional(parser, ParseParenthesizedExpression)
		if err != nil {
			return nil, err
		}
		if ok {
			return parenthesizedExpression, nil
		}

		return nil, parser.Error("Expected expression")
	})
}

func ParseParenthesizedExpression(parser *syntax.Parser) (database.Node, *syntax.Error) {
	_, err := parser.Token("LeftParenthesis", syntax.TokenConfig{
		Reason: "between these parentheses",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	value, err := ParseExpression(parser)
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	_, err = parser.Token("RightParenthesis")
	if err != nil {
		return nil, err
	}

	return value, nil
}

func visitExpression(visitor *visit.Visitor, node database.Node) {
	database.SetFact(node, IsExpressionFact{})
	database.SetFact(node, typecheck.TypedFact{})
}
