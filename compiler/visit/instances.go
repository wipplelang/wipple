package visit

import (
	"fmt"
	"reflect"
	"slices"

	"wipple/database"
	"wipple/typecheck"
)

type OverlappingInstancesFact []database.Node

func (fact OverlappingInstancesFact) String() string {
	return fmt.Sprintf("has %d overlapping instances", len(fact))
}

func CheckForOverlappingInstances(db *database.Db, traitDefinition database.Node, instances typecheck.InstancesFact) {
	solver := typecheck.NewSolver(db)

	defaultInstances := make([]typecheck.Instance, 0, len(instances))
	nonDefaultInstances := make([]typecheck.Instance, 0, len(instances))
	for _, instance := range instances {
		// Instantiate the instance to bring all referenced types into the solver
		solver.Constraints.Add(typecheck.NewInstantiateConstraint(typecheck.Instantiation{
			Source:        instance.Node,
			Definition:    instance.Node,
			Replacements:  map[database.Node]database.Node{},
			Substitutions: &map[database.Node]typecheck.Type{},
			KeepGeneric:   true,
		}, GetDefinitionConstraints))

		if instance.Default {
			defaultInstances = append(defaultInstances, instance)
		} else {
			nonDefaultInstances = append(nonDefaultInstances, instance)
		}
	}

	solver.RunPassUntil(reflect.TypeFor[*typecheck.BoundConstraint]())

	for _, instances := range [][]typecheck.Instance{defaultInstances, nonDefaultInstances} {
		var overlapping []database.Node
		for i, leftInstance := range instances {
			for _, rightInstance := range instances[i+1:] {
				copy := typecheck.CopySolver(solver)

				typecheck.UnifySubstitutions(leftInstance.Substitutions, rightInstance.Substitutions, copy)

				if !copy.Error {
					if !slices.Contains(overlapping, leftInstance.Node) {
						overlapping = append(overlapping, leftInstance.Node)
					}

					if !slices.Contains(overlapping, rightInstance.Node) {
						overlapping = append(overlapping, rightInstance.Node)
					}
				}
			}
		}

		if len(overlapping) > 0 {
			database.SetFact(traitDefinition, OverlappingInstancesFact(overlapping))
		}
	}
}
