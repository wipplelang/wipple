package patterns

import (
	"slices"

	"wipple/database"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/visit"
)

type IsPatternFact struct{}

func (fact IsPatternFact) String() string {
	return "is a pattern"
}

type PatternTemporaries interface {
	EachTemporary(f func(database.Node))
}

func ParsePattern(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		tuplePattern, ok, err := syntax.ParseOptional(parser, ParseTuplePattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return tuplePattern, nil
		}

		orPattern, ok, err := syntax.ParseOptional(parser, ParseOrPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return orPattern, nil
		}

		annotatePattern, ok, err := syntax.ParseOptional(parser, ParseAnnotatePattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return annotatePattern, nil
		}

		patternElement, ok, err := syntax.ParseOptional(parser, ParsePatternElement)
		if err != nil {
			return nil, err
		}
		if ok {
			return patternElement, nil
		}

		return nil, parser.Error("Expected pattern")
	})
}

func ParsePatternElement(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		structurePattern, ok, err := syntax.ParseOptional(parser, ParseStructurePattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return structurePattern, nil
		}

		parameterizedPattern, ok, err := syntax.ParseOptional(parser, ParseParameterizedConstructorPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return parameterizedPattern, nil
		}

		setPattern, ok, err := syntax.ParseOptional(parser, ParseSetPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return setPattern, nil
		}

		atomicPattern, ok, err := syntax.ParseOptional(parser, ParseAtomicPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return atomicPattern, nil
		}

		return nil, parser.Error("Expected pattern")
	})
}

func ParseAtomicPattern(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		constructorPattern, ok, err := syntax.ParseOptional(parser, ParseConstructorPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return constructorPattern, nil
		}

		wildcardPattern, ok, err := syntax.ParseOptional(parser, ParseWildcardPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return wildcardPattern, nil
		}

		variablePattern, ok, err := syntax.ParseOptional(parser, ParseVariablePattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return variablePattern, nil
		}

		numberPattern, ok, err := syntax.ParseOptional(parser, ParseNumberPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return numberPattern, nil
		}

		stringPattern, ok, err := syntax.ParseOptional(parser, ParseStringPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return stringPattern, nil
		}

		unitPattern, ok, err := syntax.ParseOptional(parser, ParseUnitPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return unitPattern, nil
		}

		parenthesizedPattern, ok, err := syntax.ParseOptional(parser, ParseParenthesizedPattern)
		if err != nil {
			return nil, err
		}
		if ok {
			return parenthesizedPattern, nil
		}

		return nil, parser.Error("Expected pattern")
	})
}

func ParseParenthesizedPattern(parser *syntax.Parser) (database.Node, *syntax.Error) {
	span := parser.Spanned()

	_, err := parser.Token("LeftParenthesis", syntax.TokenConfig{
		Reason: "between these parentheses",
	})
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	value, err := ParsePattern(parser)
	if err != nil {
		return nil, err
	}

	parser.ConsumeLineBreaks()

	_, err = parser.Token("RightParenthesis")
	if err != nil {
		return nil, err
	}

	database.SetSpanFact(value, span())

	return value, nil
}

func visitPattern(visitor *visit.Visitor, node database.Node) {
	database.SetFact(node, IsPatternFact{})
	database.SetFact(node, typecheck.TypedFact{})
	visitor.Constraint(typecheck.NewGroupConstraint(node, visitor.CurrentMatch.Node))
}

type eachTemporary interface {
	EachTemporary(f func(database.Node))
}

func EachTemporary(pattern database.Node, f func(database.Node)) {
	if pattern, ok := pattern.(eachTemporary); ok {
		pattern.EachTemporary(f)
	}
}

func CollectTemporaries(patterns ...database.Node) []database.Node {
	temporaries := []database.Node{}
	for _, pattern := range patterns {
		EachTemporary(pattern, func(temporary database.Node) {
			if !slices.Contains(temporaries, temporary) {
				temporaries = append(temporaries, temporary)
			}
		})
	}

	return temporaries
}
