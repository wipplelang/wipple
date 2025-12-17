package typecheck

import (
	"fmt"
	"maps"
	"slices"

	"wipple/database"
)

func BoundConstraint(node database.Node, bound UnresolvedBound, getDefinitionConstraints func(database.Node) []Constraint) Constraint {
	return newConstraint(BoundConstraint, constraintConfig{
		Node: node,
		Instance: &Instance{
			Node:          node,
			Trait:         bound.Trait,
			Substitutions: bound.Substitutions,
		},
		Debug: func() string {
			return fmt.Sprintf("BoundConstraint(%v)", DisplayUnresolvedBound(bound))
		},
		Instantiate: func(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint {
			return BoundConstraint(node, UnresolvedBound{
				Source:        source,
				Trait:         bound.Trait,
				Substitutions: InstantiateSubstitutions(solver, bound.Substitutions, source, substitutions, replacements),
				TraitName:     bound.TraitName,
			}, getDefinitionConstraints)
		},
		Run: func(solver *Solver) bool {
			// These are for the *trait's* parameters
			boundSubstitutions := map[database.Node]Type{}
			boundInferred := map[database.Node]Type{}
			for parameter, ty := range *bound.Substitutions {
				// NOTE: No need to instantiate `type` here; the bound has
				// already been instantiated
				if _, ok := database.GetFact[InferredParameterFact](parameter); ok {
					boundInferred[parameter] = ty
				} else {
					boundSubstitutions[parameter] = ty
				}
			}

			instances, _ := database.GetFact[InstancesFact](bound.Trait)

			var nonDefaultInstances []Instance
			var defaultInstances []Instance
			for _, instance := range instances {
				if instance.Default {
					defaultInstances = append(defaultInstances, instance)
				} else {
					nonDefaultInstances = append(nonDefaultInstances, instance)
				}
			}

			instanceGroups := []struct {
				Instances   []Instance
				Instantiate bool
			}{
				{solver.ImpliedInstances, false},
				{nonDefaultInstances, true},
				{defaultInstances, true},
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
					if instance.Trait != bound.Trait {
						continue
					}

					copy := CopySolver(solver)
					copy.ImpliedInstances = append(copy.ImpliedInstances, solver.ImpliedInstances...)

					// These are for the *instance's own* parameters, not the
					// trait parameters like with the bound
					replacements := map[database.Node]database.Node{}
					substitutions := map[database.Node]Type{}
					if group.Instantiate {
						copy.Constraints.Add(InstantiateConstraint(Instantiation{
							Source:        bound.Source,
							Definition:    instance.Node,
							Replacements:  replacements,
							Substitutions: &substitutions,
						}, getDefinitionConstraints))
					}

					// Run the solver (excluding bounds) to populate
					// `replacements`
					copy.RunPassUntil(BoundConstraint)

					// These are for the *trait's* parameters
					instanceSubstitutions := map[database.Node]Type{}
					instanceInferred := map[database.Node]Type{}
					for parameter, substitution := range *instance.Substitutions {
						if group.Instantiate {
							substitution = InstantiateType(solver, substitution, node, &substitutions, replacements)
						}

						if _, ok := database.GetFact[InferredParameterFact](parameter); ok {
							instanceInferred[parameter] = substitution
						} else {
							instanceSubstitutions[parameter] = substitution
						}
					}

					copy.Error = false
					UnifySubstitutions(&instanceSubstitutions, &boundSubstitutions, copy)
					if !copy.Error {
						UnifySubstitutions(&instanceInferred, &boundInferred, copy)
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
						Source:        bound.Source,
						Trait:         bound.Trait,
						Substitutions: &resolvedSubstitutions,
						TraitName:     bound.TraitName,
					},
					Solver: solver,
				}

				// Allow multiple candidates (picking the first) if considering
				// implied instances
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

					// Don't indicate a resolved instance if this instance is
					// implied (suppresses custom `[error]` messages)
					isImpliedInstance := slices.ContainsFunc(solver.ImpliedInstances, func(existing Instance) bool {
						return existing.Node == candidate.Node
					})

					// `&& group.Instantiate` means we only record resolved
					// instance definitions
					if !isImpliedInstance && group.Instantiate {
						var fact BoundsFact
						if f, ok := database.GetFact[BoundsFact](bound.Source); ok {
							fact = f
						}

						fact = append(fact, BoundsFactItem{
							Bound:    ApplyBound(resolvedBound),
							Instance: candidate.Node,
							Error:    candidate.Error,
						})

						database.SetFact(bound.Source, fact)
					}

					break
				} else if len(candidates) > 1 {
					return false // ambiguous; try again
				}

				if lastInstanceSet {
					var fact BoundsFact
					if f, ok := database.GetFact[BoundsFact](bound.Source); ok {
						fact = f
					}

					fact = append(fact, BoundsFactItem{
						Bound: ApplyBound(resolvedBound),
					})

					database.SetFact(bound.Source, fact)
				}
			}

			return true
		},
	})
}
