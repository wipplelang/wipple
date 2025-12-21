package typecheck

import (
	"maps"
	"slices"

	"wipple/colors"
	"wipple/database"
)

type Instance struct {
	Node          database.Node
	Trait         database.Node
	Substitutions *map[database.Node]Type
	Default       bool
	Error         bool
}

type InstancesFact []Instance

func (fact InstancesFact) String() string {
	return "has instances"
}

type UnresolvedBound struct {
	Source        database.Node
	Trait         database.Node
	Substitutions *map[database.Node]Type
	TraitName     string
	Optional      bool
}

type ResolvedBound struct {
	UnresolvedBound
	Solver *Solver
}

func (b ResolvedBound) Clone() ResolvedBound {
	substitutions := maps.Clone(*b.Substitutions)

	return ResolvedBound{
		UnresolvedBound: UnresolvedBound{
			Source:        b.Source,
			Trait:         b.Trait,
			Substitutions: &substitutions,
			TraitName:     b.TraitName,
			Optional:      b.Optional,
		},
		Solver: b.Solver,
	}
}

func DisplayResolvedBound(b ResolvedBound) string {
	b = ApplyBound(b)
	return DisplayUnresolvedBound(b.UnresolvedBound)
}

func DisplayUnresolvedBound(b UnresolvedBound) string {
	s := b.TraitName

	parameters := make([]database.Node, 0, len(*b.Substitutions))
	for parameter := range *b.Substitutions {
		parameters = append(parameters, parameter)
	}

	slices.SortStableFunc(parameters, func(left database.Node, right database.Node) int {
		return database.CompareSpans(database.GetSpanFact(left), database.GetSpanFact(right))
	})

	for _, parameter := range parameters {
		s += " "
		s += DisplayType((*b.Substitutions)[parameter], false)
	}

	return s
}

func ApplyBound(b ResolvedBound) ResolvedBound {
	return ResolvedBound{
		UnresolvedBound: UnresolvedBound{
			Source:        b.Source,
			Trait:         b.Trait,
			Substitutions: ApplySubstitutions(b.Substitutions, b.Solver),
			TraitName:     b.TraitName,
			Optional:      b.Optional,
		},
		Solver: b.Solver,
	}
}

type BoundsFact []BoundsFactItem

type BoundsFactItem struct {
	Bound    ResolvedBound
	Instance database.Node
	Error    bool
}

func (fact BoundsFact) String() string {
	if len(fact) == 0 {
		return "has bound(s)"
	}

	s := "has bound(s) "
	for i, item := range fact {
		if i > 0 {
			s += ", "
		}

		s += colors.Code(DisplayResolvedBound(ApplyBound(item.Bound)))

		s += " ("
		if item.Instance != nil {
			s += database.DisplayNode(item.Instance)
		} else {
			s += "unresolved"
		}

		s += ")"
	}

	return s
}
