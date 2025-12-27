package typecheck

import (
	"fmt"
	"slices"
	"strings"

	"wipple/colors"
	"wipple/database"
)

type TypedFact struct {
	Group *Group
}

func (fact TypedFact) String() string {
	if fact.Group == nil {
		return "types not solved"
	}

	if len(fact.Group.Types) == 0 {
		var s strings.Builder
		s.WriteString("missing type")

		if len(fact.Group.Nodes) > 1 {
			s.WriteString(" (group: ")

			i := 0
			for _, node := range fact.Group.Nodes {
				if database.IsHiddenNode(node) {
					continue
				}

				if i > 0 {
					s.WriteString(", ")
				}

				s.WriteString(database.DisplayNode(node))

				i++
			}

			s.WriteString(")")
		}

		return s.String()
	}

	var s strings.Builder
	s.WriteString("has type")

	for i, ty := range fact.Group.Types {
		if i > 0 {
			s.WriteString(colors.Conflict(" or"))
		}

		s.WriteString(" " + colors.Code(DisplayType(ty, true)))
	}

	return s.String()
}

type InstantiatedFact struct {
	Definition database.Node
	From       database.Node
	Source     database.Node
}

func (fact InstantiatedFact) String() string {
	return ""
}

type ConstructedType struct {
	Tag         any
	Children    []Type
	Instantiate database.Node
	Display     func(children []func(root bool) string, root bool) string
	Codegen     func(children []any, node func(node database.Node) string) any
}

type Type any

func CloneType(ty Type) Type {
	switch ty := ty.(type) {
	case *ConstructedType:
		if ty.Instantiate != nil {
			return ty
		}

		children := make([]Type, len(ty.Children))
		for i, child := range ty.Children {
			children[i] = CloneType(child)
		}

		return replaceChildren(ty, children)
	case database.Node:
		return ty
	default:
		return nil
	}
}

func DisplayType(ty Type, root bool) string {
	switch ty := ty.(type) {
	case *ConstructedType:
		children := make([]func(root bool) string, len(ty.Children))
		for i, child := range ty.Children {
			children[i] = func(root bool) string {
				return DisplayType(child, root)
			}
		}

		return ty.Display(children, root)
	case database.Node:
		return "_"
	default:
		panic(fmt.Sprintf("invalid type: %T", ty))
	}
}

func TraverseType(ty Type, f func(Type) (Type, bool)) Type {
	var traverse func(ty Type, stack *[]Type) Type
	traverse = func(ty Type, stack *[]Type) Type {
		ty, done := f(ty)
		if done {
			return ty
		}

		if slices.Contains(*stack, ty) {
			return ty // recursive type
		}

		*stack = append(*stack, ty)

		result := ty
		switch ty := ty.(type) {
		case *ConstructedType:
			if ty.Instantiate != nil {
				break // ignore the children of type parameters
			}

			children := make([]Type, 0, len(ty.Children))
			for _, child := range ty.Children {
				children = append(children, traverse(child, stack))
			}

			result = replaceChildren(ty, children)
		}

		*stack = (*stack)[:len(*stack)-1]

		return result
	}

	var stack []Type
	return traverse(ty, &stack)
}

func TypeReferencesNode(ty Type, nodes ...database.Node) bool {
	referencesNode := false
	TraverseType(ty, func(ty Type) (Type, bool) {
		node, ok := ty.(database.Node)
		if !ok {
			return ty, false
		}

		referencesNode = len(nodes) == 0 || slices.ContainsFunc(nodes, func(n database.Node) bool {
			return n == node
		})

		return ty, referencesNode
	})

	return referencesNode
}

func TypesAreEqual(left Type, right Type) bool {
	if left == right {
		return true
	}

	if left, ok := left.(*ConstructedType); ok {
		if right, ok := right.(*ConstructedType); ok {
			if left.Instantiate != right.Instantiate {
				return false
			}

			return left.Tag == right.Tag &&
				len(left.Children) == len(right.Children) &&
				slices.EqualFunc(left.Children, right.Children, TypesAreEqual)
		}
	}

	return false
}

func InstantiateType(solver *Solver, ty Type, definition database.Node, source database.Node, substitutions *map[database.Node]Type, replacements map[database.Node]database.Node) Type {
	return TraverseType(ty, func(ty Type) (Type, bool) {
		switch ty := ty.(type) {
		case database.Node:
			return GetOrInstantiate(solver, ty, definition, source, replacements), false
		case *ConstructedType:
			if ty.Instantiate != nil {
				parameter := ty.Instantiate

				if substitution, ok := (*substitutions)[parameter]; ok {
					return substitution, false
				}

				substitution := &database.HiddenNode{
					Facts: database.NewFacts(database.GetSpanFact(parameter)),
				}
				solver.Db.Register(substitution)
				database.SetParentFact(substitution, database.GetParentFact(parameter))
				database.SetFact(substitution, TypedFact{})

				database.SetFact(substitution, InstantiatedFact{
					Definition: definition,
					From:       parameter,
					Source:     source,
				})

				(*substitutions)[parameter] = substitution

				return substitution, false
			} else {
				return ty, false
			}
		default:
			return nil, false
		}
	})
}

func GetOrInstantiate(solver *Solver, node database.Node, definition database.Node, source database.Node, replacements map[database.Node]database.Node) database.Node {
	replacement, ok := replacements[node]
	if !ok {
		instantiated := &database.HiddenNode{
			Facts: database.NewFacts(database.GetSpanFact(node)),
		}
		solver.Db.Register(instantiated)
		database.SetParentFact(instantiated, database.GetParentFact(node))
		database.SetFact(instantiated, TypedFact{})

		replacement = instantiated
		replacements[node] = instantiated
	}

	database.SetFact(replacement, InstantiatedFact{
		Definition: definition,
		From:       node,
		Source:     source,
	})

	solver.Db.Graph.Replace(node, replacement)

	return replacement

}

func replaceChildren(ty *ConstructedType, children []Type) *ConstructedType {
	return &ConstructedType{
		Tag:         ty.Tag,
		Children:    children,
		Instantiate: ty.Instantiate,
		Display:     ty.Display,
		Codegen:     ty.Codegen,
	}
}
