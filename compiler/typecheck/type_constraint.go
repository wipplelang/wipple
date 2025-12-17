package typecheck

import (
	"fmt"

	"wipple/database"
)

func TypeConstraint(node database.Node, ty *ConstructedType) Constraint {
	return newConstraint(TypeConstraint, constraintConfig{
		Node: node,
		Debug: func() string {
			return fmt.Sprintf("TypeConstraint(%v :: %v)", database.DisplayNode(node), DisplayType(ty, true))
		},
		Instantiate: func(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
			node := GetOrInstantiate(solver, node, source, replacements)
			ty := InstantiateType(solver, ty, source, substitutions, replacements)

			switch ty := ty.(type) {
			case database.Node:
				return GroupConstraint(node, ty)
			case *ConstructedType:
				return TypeConstraint(node, ty)
			default:
				panic(fmt.Sprintf("invalid type: %T", ty))
			}
		},
		Run: func(solver *Solver) bool {
			solver.Unify(node, ty)
			return true
		},
	})
}
