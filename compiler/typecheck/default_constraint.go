package typecheck

import (
	"fmt"

	"wipple/database"
)

type DefaultConstraint struct {
	info *ConstraintInfo
	Ty   Type
}

func (c *DefaultConstraint) Info() *ConstraintInfo {
	return c.info
}

func (c *DefaultConstraint) String() string {
	return fmt.Sprintf("DefaultConstraint(%v :: %v)", database.DisplayNode(c.info.Node), DisplayType(c.Ty, true))
}

func (c *DefaultConstraint) Instantiate(solver *Solver, source database.Node, definition database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
	node := GetOrInstantiate(solver, c.info.Node, definition, source, replacements)
	ty := InstantiateType(solver, c.Ty, definition, source, substitutions, replacements)

	return NewDefaultConstraint(node, ty)
}

func (c *DefaultConstraint) Run(solver *Solver) bool {
	if _, ok := solver.Apply(c.info.Node).(database.Node); ok {
		solver.Unify(c, c.info.Node, c.Ty)
	}

	return true
}

func NewDefaultConstraint(node database.Node, ty Type) *DefaultConstraint {
	return &DefaultConstraint{
		Ty:   ty,
		info: DefaultConstraintInfo(node, database.GetSpanFact(node)),
	}
}
