package typecheck

import (
	"fmt"

	"wipple/database"
)

type TypeConstraint struct {
	info *ConstraintInfo
	Type *ConstructedType
}

func (c *TypeConstraint) Info() *ConstraintInfo {
	return c.info
}

func (c *TypeConstraint) String() string {
	return fmt.Sprintf("TypeConstraint(%v :: %v)", database.DisplayNode(c.info.Node), DisplayType(c.Type, true))
}

func (c *TypeConstraint) Instantiate(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
	node := GetOrInstantiate(solver, c.info.Node, source, replacements)
	ty := InstantiateType(solver, c.Type, source, substitutions, replacements)

	switch ty := ty.(type) {
	case database.Node:
		return NewGroupConstraint(node, ty)
	case *ConstructedType:
		return NewTypeConstraint(node, ty)
	default:
		panic(fmt.Sprintf("invalid type: %T", ty))
	}
}

func (c *TypeConstraint) Run(solver *Solver) bool {
	solver.Unify(c, c.info.Node, c.Type)
	return true
}

func NewTypeConstraint(node database.Node, ty *ConstructedType) *TypeConstraint {
	return &TypeConstraint{
		info: DefaultConstraintInfo(node),
		Type: ty,
	}
}
