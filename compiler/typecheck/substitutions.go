package typecheck

import "wipple/database"

type InferredParameterFact struct{}

func (fact InferredParameterFact) String() string {
	return "is inferred type parameter"
}

func UnifySubstitutions(trace Constraint, left *map[database.Node]Type, right *map[database.Node]Type, solver *Solver) {
	for parameter, leftType := range *left {
		if rightType, ok := (*right)[parameter]; ok {
			solver.Unify(trace, leftType, rightType)
		}
	}
}

func ApplySubstitutions(s *map[database.Node]Type, solver *Solver) *map[database.Node]Type {
	applied := make(map[database.Node]Type, len(*s))
	for parameter, ty := range *s {
		applied[parameter] = solver.Apply(ty)
	}

	return &applied
}

func InstantiateSubstitutions(solver *Solver, s *map[database.Node]Type, definition database.Node, source database.Node, substitutions *map[database.Node]Type, replacements map[database.Node]database.Node) *map[database.Node]Type {
	instantiated := map[database.Node]Type{}
	for parameter, ty := range *s {
		instantiated[parameter] = InstantiateType(solver, ty, definition, source, substitutions, replacements)
	}

	return &instantiated
}
