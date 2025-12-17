package typecheck

import (
	"fmt"

	"wipple/database"
)

func DefaultConstraint(node database.Node, ty Type) Constraint {
	return newConstraint(DefaultConstraint, constraintConfig{
		Node: node,
		Debug: func() string {
			return fmt.Sprintf("DefaultConstraint(%v :: %v)", database.DisplayNode(node), DisplayType(ty, true))
		},
		Instantiate: func(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
			node := GetOrInstantiate(solver, node, source, replacements)
			ty := InstantiateType(solver, ty, source, substitutions, replacements)

			return DefaultConstraint(node, ty)
		},
		Run: func(solver *Solver) bool {
			if _, ok := solver.Apply(node).(database.Node); ok {
				solver.Unify(node, ty)
			}

			return true
		},
	})
}
