package typecheck

import (
	"reflect"
	"slices"
)

var constraintOrder []*[]reflect.Value

func init() {
	constraintOrder = []*[]reflect.Value{
		{reflect.ValueOf(GroupConstraint)},
		{reflect.ValueOf(TypeConstraint)},
		{reflect.ValueOf(InstantiateConstraint)},
		{reflect.ValueOf(BoundConstraint), reflect.ValueOf(DefaultConstraint)},
	}
}

type Constraints struct {
	constraints map[*[]reflect.Value][]Constraint
}

func (c *Constraints) Add(constraints ...Constraint) {
	if c.constraints == nil {
		c.constraints = make(map[*[]reflect.Value][]Constraint, len(constraintOrder))
	}

	for _, constraint := range constraints {
		var key *[]reflect.Value
		for _, group := range constraintOrder {
			if slices.Contains(*group, constraint.key) {
				key = group
				break
			}
		}

		if key == nil {
			panic("unknown constraint type")
		}

		if _, ok := c.constraints[key]; !ok {
			c.constraints[key] = []Constraint{}
		}

		c.constraints[key] = append(c.constraints[key], constraint)
	}
}

func (c *Constraints) RunUntil(solver *Solver, stop any) {
	expectedConstraints := 0
	for _, constraints := range c.constraints {
		expectedConstraints += len(constraints)
	}

	requeuedConstraints := make([]Constraint, 0, expectedConstraints)
	for {
		constraint, ok := c.dequeue(stop)
		if !ok {
			break
		}

		if constraint.IsActive {
			ok := constraint.run(solver)
			if !ok {
				requeuedConstraints = append(requeuedConstraints, constraint)
			}
		}
	}

	c.Add(requeuedConstraints...)
}

func (c *Constraints) All() []Constraint {
	total := 0
	for _, key := range constraintOrder {
		if constraints, ok := c.constraints[key]; ok {
			total += len(constraints)
		}
	}

	all := make([]Constraint, 0, total)
	for _, key := range constraintOrder {
		if constraints, ok := c.constraints[key]; ok {
			all = append(all, constraints...)
		}
	}

	return all
}

func (c *Constraints) dequeue(stop any) (Constraint, bool) {
	for _, key := range constraintOrder {
		if constraints, ok := c.constraints[key]; ok && len(constraints) > 0 {
			constraint := constraints[0]

			if stop != nil && constraint.key == reflect.ValueOf(stop) {
				return Constraint{}, false
			}

			c.constraints[key] = constraints[1:]
			return constraint, true
		}
	}

	return Constraint{}, false
}
