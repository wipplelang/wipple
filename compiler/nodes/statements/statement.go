package statements

import (
	"wipple/database"
	"wipple/syntax"
)

type ParseExpressionFunc func(*syntax.Parser) (database.Node, *syntax.Error)

func ParseStatements(parser *syntax.Parser, parseExpression ParseExpressionFunc) ([]database.Node, *syntax.Error) {
	lines, err := syntax.ParseLines(parser, 0, true, func(parser *syntax.Parser) (database.Node, *syntax.Error) {
		statement, err := ParseStatement(parser, parseExpression)
		if err != nil {
			return nil, err
		}

		// Trailing comments
		_, _, err = syntax.ParseOptional(parser, ParseComment)
		if err != nil {
			return nil, err
		}

		return statement, nil
	})
	if err != nil {
		return nil, err
	}

	return lines, nil
}

func ParseStatement(parser *syntax.Parser, parseExpression ParseExpressionFunc) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		typeDefinition, ok, err := syntax.ParseOptional(parser, ParseTypeDefinitionStatement)
		if err != nil {
			return nil, err
		}
		if ok {
			return typeDefinition, nil
		}

		traitDefinition, ok, err := syntax.ParseOptional(parser, ParseTraitDefinitionStatement)
		if err != nil {
			return nil, err
		}
		if ok {
			return traitDefinition, nil
		}

		constantDefinition, ok, err := syntax.ParseOptional(parser, ParseConstantDefinitionStatement)
		if err != nil {
			return nil, err
		}
		if ok {
			return constantDefinition, nil
		}

		instanceDefinition, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (*InstanceDefinitionNode, *syntax.Error) {
			return ParseInstanceDefinitionStatement(parser, parseExpression)
		})
		if err != nil {
			return nil, err
		}
		if ok {
			return instanceDefinition, nil
		}

		assignment, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (*AssignmentNode, *syntax.Error) {
			return ParseAssignmentStatement(parser, parseExpression)
		})
		if err != nil {
			return nil, err
		}
		if ok {
			return assignment, nil
		}

		expression, ok, err := syntax.ParseOptional(parser, func(parser *syntax.Parser) (*ExpressionStatementNode, *syntax.Error) {
			return ParseExpressionStatement(parser, parseExpression)
		})
		if err != nil {
			return nil, err
		}
		if ok {
			return expression, nil
		}

		return nil, parser.Error("Expected statement")
	})
}

func ParseComments(parser *syntax.Parser) ([]string, *syntax.Error) {
	lines, err := syntax.ParseLines(parser, 0, true, ParseComment)
	if err != nil {
		return nil, err
	}

	return lines, nil
}

func ParseComment(parser *syntax.Parser) (string, *syntax.Error) {
	comment, err := parser.Token("Comment")
	if err != nil {
		return "", err
	}

	return comment, nil
}
