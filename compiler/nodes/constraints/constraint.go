package constraints

import (
	"wipple/database"
	"wipple/syntax"
	"wipple/visit"
)

type IsConstraintFact struct{}

func (fact IsConstraintFact) String() string {
	return "is a constraint"
}

func ParseConstraints(parser *syntax.Parser) ([]database.Node, *syntax.Error) {
	_, err := parser.Token("WhereKeyword")
	if err != nil {
		return nil, err
	}

	parser.Commit("in these constraints")

	many, err := syntax.ParseMany(parser, 0, ParseConstraint, syntax.ParseNothing)
	if err != nil {
		return nil, err
	}

	constraints := make([]database.Node, 0, len(many))
	for _, item := range many {
		constraints = append(constraints, item.Value)
	}

	return constraints, nil
}

func ParseConstraint(parser *syntax.Parser) (database.Node, *syntax.Error) {
	return syntax.ParseCached(parser, func(p *syntax.Parser) (database.Node, *syntax.Error) {
		boundConstraint, ok, err := syntax.ParseOptional(parser, ParseBoundConstraint)
		if err != nil {
			return nil, err
		}
		if ok {
			return boundConstraint, nil
		}

		defaultConstraint, ok, err := syntax.ParseOptional(parser, ParseDefaultConstraint)
		if err != nil {
			return nil, err
		}
		if ok {
			return defaultConstraint, nil
		}

		return nil, parser.Error("Expected constraint")
	})
}

func visitConstraint(visitor *visit.Visitor, node database.Node) {
	database.SetFact(node, IsConstraintFact{})
}
