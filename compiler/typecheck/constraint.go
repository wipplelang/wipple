package typecheck

import (
	"fmt"
	"wipple/database"
)

type Constraint interface {
	fmt.Stringer
	Info() *ConstraintInfo
	Instantiate(solver *Solver, definition database.Node, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint
	Run(solver *Solver) bool
}

type ConstraintInfo struct {
	Node              database.Node
	Span              database.Span
	Instance          *Instance
	IsActive          bool
	ShouldInstantiate bool
}

func DefaultConstraintInfo(node database.Node, span database.Span) *ConstraintInfo {
	return &ConstraintInfo{
		Node:              node,
		Span:              span,
		IsActive:          true,
		ShouldInstantiate: true,
	}
}
