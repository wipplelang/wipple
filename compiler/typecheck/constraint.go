package typecheck

import (
	"fmt"
	"wipple/database"
)

type Constraint interface {
	fmt.Stringer
	Info() *ConstraintInfo
	Instantiate(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint
	Run(solver *Solver) bool
}

type ConstraintInfo struct {
	Node              database.Node
	Instance          *Instance
	IsActive          bool
	ShouldInstantiate bool
}

func DefaultConstraintInfo(node database.Node) *ConstraintInfo {
	return &ConstraintInfo{
		Node:              node,
		IsActive:          true,
		ShouldInstantiate: true,
	}
}
