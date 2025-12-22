package feedback

import (
	"fmt"
	"slices"
	"wipple/database"
	"wipple/nodes/expressions"
	"wipple/queries"
	"wipple/typecheck"
)

func registerFunctions() {
	findCall := func(nodes []database.Node) (*expressions.CallExpressionNode, bool) {
		for _, node := range nodes {
			if parent := database.GetParentFact(node); parent != nil {
				if call, ok := parent.(*expressions.CallExpressionNode); ok {
					return call, true
				}
			}
		}

		return nil, false
	}

	type missingInputsData struct {
		Source   database.Node
		Function database.Node
		Nodes    []database.Node
		Inputs   []typecheck.Type
	}

	register(Feedback[missingInputsData]{
		Id:   "missing-inputs",
		Rank: RankSyntax,
		Query: func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data missingInputsData)) {
			queries.ConflictingTypes(db, node, filter, func(source database.Node, from database.Node, nodes []database.Node, types []*typecheck.ConstructedType, trace []typecheck.Constraint) {
				nodes = append([]database.Node{source}, nodes...)

				call, ok := findCall(nodes)
				if !ok {
					return
				}

				if len(types) == 2 && types[0].Tag == "function" && types[1].Tag == "function" && len(types[0].Children) < len(types[1].Children) {
					f(missingInputsData{
						Source:   source,
						Function: call.Function,
						Nodes:    nodes,
						Inputs:   types[1].Children[len(types[0].Children):],
					})
				}
			})
		},
		On: func(data missingInputsData) []database.Node {
			nodes := []database.Node{data.Function}
			for _, node := range data.Nodes {
				if !slices.Contains(nodes, node) {
					nodes = append(nodes, node)
				}
			}
			nodes = append(nodes, data.Source)

			return nodes
		},
		Render: func(render *Render, node database.Node, data missingInputsData) {
			render.WriteNode(data.Function)
			render.WriteString(" is missing ")

			items := make([]func(), len(data.Inputs))
			for i, input := range data.Inputs {
				items[i] = func() {
					if _, ok := input.(*typecheck.ConstructedType); ok {
						render.WriteString("a ")
						render.WriteType(input)
					} else {
						render.WriteString("an input")
					}
				}
			}
			render.WriteList(items, "and a", -1)
			render.WriteString(".")

			render.WriteBreak()
			render.WriteString("Try adding ")
			if len(data.Inputs) == 1 {
				render.WriteString("this input")
			} else {
				render.WriteString("these inputs")
			}
			render.WriteString(", or double-check your parentheses.")
		},
	})

	type extraInputsData struct {
		Source   database.Node
		Function database.Node
		Nodes    []database.Node
		Actual   int
		Expected int
	}

	register(Feedback[extraInputsData]{
		Id:   "extra-input",
		Rank: RankSyntax,
		Query: func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data extraInputsData)) {
			queries.ConflictingTypes(db, node, filter, func(source database.Node, from database.Node, nodes []database.Node, types []*typecheck.ConstructedType, trace []typecheck.Constraint) {
				nodes = append([]database.Node{source}, nodes...)

				call, ok := findCall(nodes)
				if !ok {
					return
				}

				if len(types) == 2 && types[0].Tag == "function" && types[1].Tag == "function" && len(types[0].Children) > len(types[1].Children) {
					f(extraInputsData{
						Source:   source,
						Function: call.Function,
						Nodes:    nodes,
						Actual:   len(types[0].Children) - 1,
						Expected: len(types[1].Children) - 1,
					})
				}
			})
		},
		On: func(data extraInputsData) []database.Node {
			nodes := []database.Node{data.Function}
			for _, node := range data.Nodes {
				if !slices.Contains(nodes, node) {
					nodes = append(nodes, node)
				}
			}
			nodes = append(nodes, data.Source)

			return nodes
		},
		Render: func(render *Render, node database.Node, data extraInputsData) {
			render.WriteNode(data.Function)
			render.WriteString(" only needs ")
			render.WriteNumber(data.Expected, "input", "inputs")
			render.WriteString(".")

			render.WriteBreak()
			render.WriteString("Try removing ")
			if data.Actual-data.Expected == 1 {
				render.WriteString("the extra input")
			} else {
				render.WriteString(fmt.Sprintf("%d extra inputs", data.Actual-data.Expected))
			}
			render.WriteString(" here.")
		},
	})

	type notAFunctionData struct {
		Source   database.Node
		Function database.Node
		Nodes    []database.Node
	}

	register(Feedback[notAFunctionData]{
		Id:   "not-a-function",
		Rank: RankSyntax,
		Query: func(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(data notAFunctionData)) {
			queries.ConflictingTypes(db, node, filter, func(source database.Node, from database.Node, nodes []database.Node, types []*typecheck.ConstructedType, trace []typecheck.Constraint) {
				nodes = append([]database.Node{source}, nodes...)

				call, ok := findCall(nodes)
				if !ok {
					return
				}

				if len(types) == 2 && (types[0].Tag == "function") != (types[1].Tag == "function") {
					f(notAFunctionData{
						Source:   source,
						Function: call.Function,
						Nodes:    nodes,
					})
				}
			})
		},
		On: func(data notAFunctionData) []database.Node {
			nodes := []database.Node{data.Function}
			for _, node := range data.Nodes {
				if !slices.Contains(nodes, node) {
					nodes = append(nodes, node)
				}
			}
			nodes = append(nodes, data.Source)

			return nodes
		},
		Render: func(render *Render, node database.Node, data notAFunctionData) {
			render.WriteNode(data.Function)
			render.WriteString(" is not a function.")

			render.WriteBreak()
			render.WriteString("Double-check your parentheses.")
		},
	})
}
