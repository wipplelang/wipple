package typecheck

import (
	"fmt"

	"wipple/database"
)

func GroupConstraint(left database.Node, right database.Node) Constraint {
	return newConstraint(GroupConstraint, constraintConfig{
		Debug: func() string {
			return fmt.Sprintf("GroupConstraint(%v == %v)", database.DisplayNode(left), database.DisplayNode(right))
		},
		Instantiate: func(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
			left := GetOrInstantiate(solver, left, source, replacements)
			right := GetOrInstantiate(solver, right, source, replacements)

			return GroupConstraint(left, right)
		},
		Run: func(solver *Solver) bool {
			solver.Unify(left, right)
			return true
		},
	})
}
