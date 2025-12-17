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

func InstantiateConstraint(i Instantiation, getDefinitionConstraints func(database.Node) []Constraint) Constraint {
	return newConstraint(InstantiateConstraint, constraintConfig{
		Debug: func() string {
			return "InstantiateConstraint(...)"
		},
		Instantiate: func(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
			newReplacements := make(map[database.Node]database.Node, len(i.Replacements))
			for node, replacement := range i.Replacements {
				newReplacements[node] = GetOrInstantiate(solver, replacement, source, replacements)
			}

			newSubstitutions := make(map[database.Node]Type, len(*i.Substitutions))
			for node, substitution := range *i.Substitutions {
				newSubstitutions[node] = InstantiateType(solver, substitution, source, substitutions, replacements)
			}

			return InstantiateConstraint(Instantiation{
				Source:        source,
				Definition:    i.Definition,
				Replacements:  newReplacements,
				Substitutions: &newSubstitutions,
			}, getDefinitionConstraints)
		},
		Run: func(solver *Solver) bool {
			// NOTE: Types are *not* applied before instantiating; we have
			// access to all related nodes/constraints here, which together will
			// form better groups

			definitionConstraints := getDefinitionConstraints(i.Definition)

			constraints := make([]Constraint, 0, len(definitionConstraints))
			for _, constraint := range definitionConstraints {
				if !constraint.ShouldInstantiate {
					continue
				}

				if !i.KeepGeneric {
					constraint = constraint.instantiate(solver, i.Source, i.Replacements, i.Substitutions)
				}

				constraints = append(constraints, constraint)
			}

			solver.Constraints.Add(constraints...)

			return true
		},
	})
}
