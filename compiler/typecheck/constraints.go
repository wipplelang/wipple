package typecheck

import (
	"reflect"
	"slices"
)

var constraintOrder = []*[]reflect.Type{
	{reflect.TypeFor[*GroupConstraint]()},
	{reflect.TypeFor[*TypeConstraint]()},
	{reflect.TypeFor[*InstantiateConstraint]()},
	{reflect.TypeFor[*BoundConstraint](), reflect.TypeFor[*DefaultConstraint]()},
}

type Constraints struct {
	constraints map[*[]reflect.Type][]Constraint
}

func (c *Constraints) Add(constraints ...Constraint) {
	if c.constraints == nil {
		c.constraints = make(map[*[]reflect.Type][]Constraint, len(constraintOrder))
	}

	for _, constraint := range constraints {
		var key *[]reflect.Type
		for _, group := range constraintOrder {
			if slices.Contains(*group, reflect.TypeOf(constraint)) {
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

func (c *Constraints) RunUntil(solver *Solver, stop reflect.Type) {
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

		if constraint.Info().IsActive {
			ok := constraint.Run(solver)
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

func (c *Constraints) dequeue(stop reflect.Type) (Constraint, bool) {
	for _, key := range constraintOrder {
		if constraints, ok := c.constraints[key]; ok && len(constraints) > 0 {
			constraint := constraints[0]

			if stop != nil && reflect.TypeOf(constraint) == stop {
				return nil, false
			}

			c.constraints[key] = constraints[1:]
			return constraint, true
		}
	}

	return nil, false
}
