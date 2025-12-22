package typecheck

import (
	"fmt"

	"wipple/database"
)

type GroupConstraint struct {
	info  *ConstraintInfo
	Left  database.Node
	Right database.Node
}

func (c *GroupConstraint) Info() *ConstraintInfo {
	return c.info
}

func (c *GroupConstraint) String() string {
	return fmt.Sprintf("GroupConstraint(%v == %v)", database.DisplayNode(c.Left), database.DisplayNode(c.Right))
}

func (c *GroupConstraint) Instantiate(solver *Solver, definition database.Node, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
	left := GetOrInstantiate(solver, c.Left, definition, source, replacements)
	right := GetOrInstantiate(solver, c.Right, definition, source, replacements)

	constraint := NewGroupConstraint(left, right)
	constraint.info.Node = source
	return constraint
}

func (c *GroupConstraint) Run(solver *Solver) bool {
	solver.Unify(c, c.Left, c.Right)
	return true
}

func NewGroupConstraint(left database.Node, right database.Node) *GroupConstraint {
	return &GroupConstraint{
		info:  DefaultConstraintInfo(nil, database.GetSpanFact(left)),
		Left:  left,
		Right: right,
	}
}
