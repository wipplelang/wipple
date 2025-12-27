package feedback

import (
	"slices"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/queries"
	"wipple/typecheck"
)

func registerTypes() {
	type conflictingTypesData struct {
		Source database.Node
		From   database.Node
		Nodes  []database.Node
		Types  []*typecheck.ConstructedType
		Trace  []typecheck.Constraint
	}

	register(Feedback[conflictingTypesData]{
		Id:        "conflicting-types",
		Rank:      RankConflicts,
		ShowGraph: true,
		Query: func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data conflictingTypesData)) {
			queries.ConflictingTypes(db, node, filter, func(source database.Node, from database.Node, nodes []database.Node, types []*typecheck.ConstructedType, trace []typecheck.Constraint) {
				f(conflictingTypesData{
					Source: source,
					From:   from,
					Nodes:  nodes,
					Types:  types,
					Trace:  trace,
				})
			})
		},
		On: func(data conflictingTypesData) []database.Node {
			nodes := []database.Node{data.Source}
			for _, node := range data.Nodes {
				if !slices.Contains(nodes, node) {
					nodes = append(nodes, node)
				}
			}

			return nodes
		},
		Render: func(render *Render, node database.Node, data conflictingTypesData) {
			if data.Source != nil && !database.HaveEqualSpans(data.Source, data.From) {
				render.WriteString("In ")
				render.WriteNode(data.Source)
				render.WriteString(", ")
			}

			var typeConstraint *typecheck.TypeConstraint
			for _, constraint := range data.Trace {
				if c, ok := constraint.(*typecheck.TypeConstraint); ok {
					typeConstraint = c
					break
				}
			}

			if typeConstraint != nil && len(data.Nodes) == 2 && len(data.Types) == 2 {
				render.WriteNode(data.From)
				render.WriteString(" isn't a ")
				render.WriteType(data.Types[1])
				render.WriteString(".")
				render.WriteBreak()
			} else {
				render.WriteNode(data.From)
				render.WriteString(" is a ")

				tys := make([]func(), 0, len(data.Types))
				for _, ty := range data.Types {
					tys = append(tys, func() {
						render.WriteTypeInGroup(ty, data.From)
					})
				}
				render.WriteList(tys, "or a", 3)

				render.WriteString(", but it can only be one of these.")
			}

			var nodes []func()
			sources := map[string]struct{}{
				database.GetSpanFact(data.From).Source: {},
			}
			for _, related := range data.Nodes {
				if _, ok := database.GetFact[types.IsTypeFact](related); !ok && !database.IsHiddenNode(related) {
					source := database.GetSpanFact(related).Source
					if _, ok := sources[source]; ok {
						continue
					}
					sources[source] = struct{}{}

					nodes = append(nodes, func() {
						render.WriteNode(related)
					})
				}
			}

			if len(nodes) > 1 {
				render.WriteBreak()
				render.WriteNode(data.From)
				render.WriteString(" must be the same type as ")
				render.WriteList(nodes, "and", 3)
				render.WriteString("; double-check these.")
			}

			if len(data.Trace) > 0 {
				seen := map[database.Node]struct{}{}
				for _, constraint := range data.Trace {
					render.WriteConstraint("\n\n  -  ", constraint, seen)
				}
			}

			var definition database.Node
			var parameter *types.TypeParameterNode
			parameterCount := 0
			for _, node := range data.Nodes {
				if fact, ok := database.GetFact[typecheck.InstantiatedFact](node); ok {
					if p, ok := fact.From.(*types.TypeParameterNode); ok {
						if parameter == nil {
							definition = fact.Definition
							parameter = p
						}

						if p.Name == parameter.Name {
							parameterCount++
						}
					}
				}
			}

			if parameterCount > 1 {
				render.WriteString("\n\n  -  ")
				render.WriteNode(parameter)
				render.WriteString(" must have the same type everywhere in this use of ")
				render.WriteNode(definition)
				render.WriteString(".")
			}
		},
	})

	register(Feedback[*typecheck.ConstructedType]{
		Id:        "incomplete-type",
		Rank:      RankUnknown,
		ShowGraph: true,
		Query:     queries.IncompleteType,
		Render: func(render *Render, node database.Node, ty *typecheck.ConstructedType) {
			render.WriteString("Missing information for the type of ")
			render.WriteNode(node)
			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Wipple determined this code is ")
			render.WriteType(ty)
			render.WriteString(", but it needs some more information for the ")
			render.WriteCode("_")
			render.WriteString(" placeholders.")
		},
	})

	register(Feedback[*typecheck.Group]{
		Id:        "unknown-type",
		Rank:      RankUnknown,
		ShowGraph: true,
		Query:     queries.UnknownType,
		Render: func(render *Render, node database.Node, group *typecheck.Group) {
			render.WriteString("Could not determine the type of ")
			render.WriteNode(node)
			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Wipple needs to know the type of this code before running it. Try using a function or assigning it to a variable.")
		},
	})

	register(Feedback[database.Node]{
		Id:    "missing-type",
		Rank:  RankSyntax,
		Query: queries.MissingType,
		Render: func(render *Render, node database.Node, parameter database.Node) {
			render.WriteNode(node)
			render.WriteString(" is missing a type for ")
			render.WriteNode(parameter)
			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Try adding another type here, or double-check your parentheses.")
		},
	})

	register(Feedback[struct{}]{
		Id:    "extra-type",
		Rank:  RankSyntax,
		Query: queries.ExtraType,
		Render: func(render *Render, node database.Node, data struct{}) {
			render.WriteNode(node)
			render.WriteString(" doesn't match any parameter of this type.")
			render.WriteBreak()
			render.WriteString("Try removing this type, or double-check your parentheses.")
		},
	})

	register(Feedback[[]database.Node]{
		Id:    "conflicting-instances",
		Rank:  RankBounds,
		Query: queries.OverlappingInstances,
		Render: func(render *Render, node database.Node, instances []database.Node) {
			render.WriteNode(node)
			render.WriteString(" has multiple overlapping instances: ")

			items := make([]func(), 0, len(instances))
			for _, instance := range instances {
				items = append(items, func() {
					render.WriteNode(instance)
				})
			}
			render.WriteList(items, "and", 3)

			render.WriteString(".")
			render.WriteBreak()
			render.WriteString("Only one of these instances can be defined at a time. Try making your instance more specific.")
		},
	})
}
