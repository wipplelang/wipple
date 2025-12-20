package typecheck

import (
	"reflect"
	"slices"
)

var constraintOrder = []*[]reflect.Type{
	{reflect.TypeFor[*GroupConstraint]()},
	{reflect.TypeFor[*TypeConstraint]()},
	{reflect.TypeFor[*InstantiateConstraint]()},
	{reflect.TypeFor[*BoundConstraint]()},
}

type Constraints struct {
	constraints        map[*[]reflect.Type][]Constraint
	defaultConstraints []*DefaultConstraint
}

func (c *Constraints) Add(constraints ...Constraint) {
	if c.constraints == nil {
		c.constraints = make(map[*[]reflect.Type][]Constraint, len(constraintOrder))
	}

	for _, constraint := range constraints {
		if constraint, ok := constraint.(*DefaultConstraint); ok {
			c.defaultConstraints = append(c.defaultConstraints, constraint)
			continue
		}

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

func (c *Constraints) RunDefaults(solver *Solver) {
	for _, constraint := range c.defaultConstraints {
		constraint.Run(solver)
	}

	c.defaultConstraints = nil
}

func (c *Constraints) All() []Constraint {
	count := len(c.defaultConstraints)
	for _, key := range constraintOrder {
		if constraints, ok := c.constraints[key]; ok {
			count += len(constraints)
		}
	}

	all := make([]Constraint, 0, count)
	for _, key := range constraintOrder {
		if constraints, ok := c.constraints[key]; ok {
			all = append(all, constraints...)
		}
	}

	for _, constraint := range c.defaultConstraints {
		all = append(all, constraint)
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
