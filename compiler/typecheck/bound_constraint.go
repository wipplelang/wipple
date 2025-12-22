package typecheck

import (
	"fmt"
	"maps"
	"reflect"
	"slices"

	"wipple/database"
)

type BoundConstraint struct {
	info                     *ConstraintInfo
	Bound                    UnresolvedBound
	GetDefinitionConstraints func(database.Node) []Constraint
	Solver                   *Solver // to display the bound
}

func (c *BoundConstraint) Info() *ConstraintInfo {
	return c.info
}

func (c *BoundConstraint) String() string {
	return fmt.Sprintf("BoundConstraint(%v)", DisplayUnresolvedBound(c.Bound))
}

func (c *BoundConstraint) Instantiate(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
	constraint := NewBoundConstraint(c.info.Node, UnresolvedBound{
		Source:        source,
		Trait:         c.Bound.Trait,
		Substitutions: InstantiateSubstitutions(solver, c.Bound.Substitutions, source, substitutions, replacements),
		TraitName:     c.Bound.TraitName,
		Optional:      c.Bound.Optional,
	}, c.GetDefinitionConstraints)

	constraint.Solver = solver

	return constraint
}

func (c *BoundConstraint) Run(solver *Solver) bool {
	c.Solver = solver

	// These are for the *trait's* parameters
	boundSubstitutions := map[database.Node]Type{}
	boundInferred := map[database.Node]Type{}
	for parameter, ty := range *c.Bound.Substitutions {
		// NOTE: No need to instantiate `type` here; the bound has already been
		// instantiated
		if _, ok := database.GetFact[InferredParameterFact](parameter); ok {
			boundInferred[parameter] = ty
		} else {
			boundSubstitutions[parameter] = ty
		}
	}

	instances, _ := database.GetFact[InstancesFact](c.Bound.Trait)

	var regularInstances []Instance
	var errorInstances []Instance
	var defaultInstances []Instance
	var defaultErrorInstances []Instance
	for _, instance := range instances {
		if instance.Error && instance.Default {
			defaultErrorInstances = append(defaultErrorInstances, instance)
		} else if instance.Error {
			errorInstances = append(errorInstances, instance)
		} else if instance.Default {
			defaultInstances = append(defaultInstances, instance)
		} else {
			regularInstances = append(regularInstances, instance)
		}
	}

	instanceGroups := []struct {
		Instances   []Instance
		Instantiate bool
	}{
		{solver.ImpliedInstances, false},
		{regularInstances, true},
		{errorInstances, true},
		{defaultInstances, true},
		{defaultErrorInstances, true},
	}

	type candidate struct {
		Solver *Solver
		Node   database.Node
		Error  bool
	}

	candidates := []candidate{}
	for i, group := range instanceGroups {
		lastInstanceSet := i+1 == len(instanceGroups)

		for _, instance := range group.Instances {
			if instance.Trait != c.Bound.Trait {
				continue
			}

			copy := CopySolver(solver)
			copy.ImpliedInstances = append(copy.ImpliedInstances, solver.ImpliedInstances...)

			// These are for the *instance's own* parameters, not the trait
			// parameters like with the bound
			replacements := map[database.Node]database.Node{}
			substitutions := map[database.Node]Type{}
			if group.Instantiate {
				copy.Constraints.Add(NewInstantiateConstraint(Instantiation{
					Source:        c.Bound.Source,
					Definition:    instance.Node,
					Replacements:  replacements,
					Substitutions: &substitutions,
				}, c.GetDefinitionConstraints))
			}

			// Run the solver (excluding bounds) to populate `replacements`
			copy.RunPassUntil(reflect.TypeFor[*BoundConstraint]())

			// These are for the *trait's* parameters
			instanceSubstitutions := map[database.Node]Type{}
			instanceInferred := map[database.Node]Type{}
			for parameter, substitution := range *instance.Substitutions {
				if group.Instantiate {
					substitution = InstantiateType(solver, substitution, c.info.Node, &substitutions, replacements)
				}

				if _, ok := database.GetFact[InferredParameterFact](parameter); ok {
					instanceInferred[parameter] = substitution
				} else {
					instanceSubstitutions[parameter] = substitution
				}
			}

			copy.Error = false
			UnifySubstitutions(c, &instanceSubstitutions, &boundSubstitutions, copy)
			if !copy.Error {
				UnifySubstitutions(c, &instanceInferred, &boundInferred, copy)
				candidates = append(candidates, candidate{
					Solver: copy,
					Node:   instance.Node,
					Error:  instance.Error,
				})
			}
		}

		resolvedSubstitutions := map[database.Node]Type{}
		maps.Copy(resolvedSubstitutions, boundSubstitutions)
		maps.Copy(resolvedSubstitutions, boundInferred)

		resolvedBound := ResolvedBound{
			UnresolvedBound: UnresolvedBound{
				Source:        c.Bound.Source,
				Trait:         c.Bound.Trait,
				Substitutions: &resolvedSubstitutions,
				TraitName:     c.Bound.TraitName,
			},
			Solver: solver,
		}

		// Allow multiple candidates (picking the first) if considering implied
		// instances
		var hasCandidate bool
		if group.Instantiate {
			hasCandidate = len(candidates) == 1
		} else {
			hasCandidate = len(candidates) > 0
		}

		if hasCandidate {
			candidate := candidates[0]

			solver.Inherit(candidate.Solver)
			solver.Constraints.Add(candidate.Solver.Constraints.All()...)

			// Don't indicate a resolved instance if this instance is implied
			// (suppresses custom `[error]` messages)
			isImpliedInstance := slices.ContainsFunc(solver.ImpliedInstances, func(existing Instance) bool {
				return existing.Node == candidate.Node
			})

			// `&& group.Instantiate` means we only record resolved instance
			// definitions
			if !isImpliedInstance && group.Instantiate {
				var fact BoundsFact
				if f, ok := database.GetFact[BoundsFact](c.Bound.Source); ok {
					fact = f
				}

				fact = append(fact, BoundsFactItem{
					Bound:    resolvedBound,
					Instance: candidate.Node,
					Error:    candidate.Error,
				})

				database.SetFact(c.Bound.Source, fact)
			}

			break
		} else if len(candidates) > 1 {
			return false // ambiguous; try again
		}

		if lastInstanceSet && !c.Bound.Optional {
			var fact BoundsFact
			if f, ok := database.GetFact[BoundsFact](c.Bound.Source); ok {
				fact = f
			}

			fact = append(fact, BoundsFactItem{Bound: resolvedBound})

			database.SetFact(c.Bound.Source, fact)
		}
	}

	return true
}

func NewBoundConstraint(node database.Node, bound UnresolvedBound, getDefinitionConstraints func(database.Node) []Constraint) *BoundConstraint {
	info := DefaultConstraintInfo(node, database.GetSpanFact(node))
	info.Instance = &Instance{
		Node:          node,
		Trait:         bound.Trait,
		Substitutions: bound.Substitutions,
	}

	return &BoundConstraint{
		info:                     info,
		Bound:                    bound,
		GetDefinitionConstraints: getDefinitionConstraints,
	}
}
