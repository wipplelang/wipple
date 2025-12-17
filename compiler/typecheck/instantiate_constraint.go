package typecheck

import (
	"wipple/database"
)

type Instantiation struct {
	Source        database.Node
	Definition    database.Node
	Replacements  map[database.Node]database.Node
	Substitutions *map[database.Node]Type
	KeepGeneric   bool // used during instance overlap checking
}

type InstantiateConstraint struct {
	info                     *ConstraintInfo
	Instantiation            Instantiation
	GetDefinitionConstraints func(database.Node) []Constraint
}

func (c *InstantiateConstraint) Info() *ConstraintInfo {
	return c.info
}

func (c *InstantiateConstraint) String() string {
	return "InstantiateConstraint(...)"
}

func (c *InstantiateConstraint) Instantiate(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
	newReplacements := make(map[database.Node]database.Node, len(c.Instantiation.Replacements))
	for node, replacement := range c.Instantiation.Replacements {
		newReplacements[node] = GetOrInstantiate(solver, replacement, source, replacements)
	}

	newSubstitutions := make(map[database.Node]Type, len(*c.Instantiation.Substitutions))
	for node, substitution := range *c.Instantiation.Substitutions {
		newSubstitutions[node] = InstantiateType(solver, substitution, source, substitutions, replacements)
	}

	return NewInstantiateConstraint(Instantiation{
		Source:        source,
		Definition:    c.Instantiation.Definition,
		Replacements:  newReplacements,
		Substitutions: &newSubstitutions,
	}, c.GetDefinitionConstraints)
}

func (c *InstantiateConstraint) Run(solver *Solver) bool {
	// NOTE: Types are *not* applied before instantiating; we have access to
	// all related nodes/constraints here, which together will form better
	// groups

	definitionConstraints := c.GetDefinitionConstraints(c.Instantiation.Definition)

	constraints := make([]Constraint, 0, len(definitionConstraints))
	for _, constraint := range definitionConstraints {
		if !constraint.Info().ShouldInstantiate {
			continue
		}

		if !c.Instantiation.KeepGeneric {
			constraint = constraint.Instantiate(solver, c.Instantiation.Source, c.Instantiation.Replacements, c.Instantiation.Substitutions)
		}

		constraints = append(constraints, constraint)
	}

	solver.Constraints.Add(constraints...)

	return true
}

func NewInstantiateConstraint(i Instantiation, getDefinitionConstraints func(database.Node) []Constraint) Constraint {
	return &InstantiateConstraint{
		info:                     DefaultConstraintInfo(nil),
		Instantiation:            i,
		GetDefinitionConstraints: getDefinitionConstraints,
	}
}
