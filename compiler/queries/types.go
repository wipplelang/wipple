package queries

import (
	"slices"
	"wipple/database"
	"wipple/nodes/types"
	"wipple/typecheck"
	"wipple/visit"
)

func Type(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(ty *typecheck.ConstructedType)) {
	if fact, ok := database.GetFact[typecheck.TypedFact](node); ok && fact.Group != nil && len(fact.Group.Types) > 0 {
		f(fact.Group.Types[0])
	}
}

func Related(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(related database.Node)) {
	if fact, ok := database.GetFact[typecheck.TypedFact](node); ok && fact.Group != nil {
		for _, related := range fact.Group.Nodes {
			if related != node {
				f(related)
			}
		}
	}
}

func ConflictingTypes(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(source database.Node, from database.Node, nodes []database.Node, types []*typecheck.ConstructedType)) {
	fact, ok := database.GetFact[typecheck.TypedFact](node)
	if !ok || fact.Group == nil {
		return
	}

	group := fact.Group
	if len(group.Types) > 1 && isFirstNodeInGroup(node, group, filter) {
		var source database.Node
		from := node
		if instantiated, ok := database.GetFact[typecheck.InstantiatedFact](node); ok {
			source = instantiated.Source
			from = instantiated.From
		}

		nodes := make([]database.Node, 0, len(group.Nodes))
		for _, other := range group.Nodes {
			if other != from && !database.IsHiddenNode(other) && filter(other) {
				nodes = append(nodes, other)
			}
		}

		seen := map[database.Span]struct{}{}
		nodes = slices.DeleteFunc(nodes, func(node database.Node) bool {
			span := database.GetSpanFact(node)
			if _, ok := seen[span]; ok {
				return true
			}

			seen[span] = struct{}{}
			return false
		})

		if source != nil {
			database.SortByProximity(nodes, source)
		} else {
			database.SortByProximity(nodes, from)
		}

		f(source, from, nodes, group.Types)
	}
}

func IncompleteType(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(ty *typecheck.ConstructedType)) {
	fact, ok := database.GetFact[typecheck.TypedFact](node)
	if !ok || fact.Group == nil {
		return
	}

	if len(fact.Group.Types) != 1 || !isFirstNodeInGroup(node, fact.Group, filter) {
		return
	}

	ty := fact.Group.Types[0]
	if typecheck.TypeReferencesNode(ty) {
		f(ty)
	}
}

func UnknownType(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(group *typecheck.Group)) {
	fact, ok := database.GetFact[typecheck.TypedFact](node)
	if !ok || fact.Group == nil {
		return
	}

	if len(fact.Group.Types) == 0 && isFirstNodeInGroup(node, fact.Group, filter) {
		f(fact.Group)
	}
}

func MissingType(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(parameter database.Node)) {
	if missing, ok := database.GetFact[types.MissingTypesFact](node); ok {
		for _, parameter := range missing {
			f(parameter)
		}
	}
}

func ExtraType(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[types.ExtraTypeFact](node); ok {
		f(struct{}{})
	}
}

func OverlappingInstances(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(instances []database.Node)) {
	if overlapping, ok := database.GetFact[visit.OverlappingInstancesFact](node); ok {
		f(overlapping)
	}
}

func isFirstNodeInGroup(node database.Node, group *typecheck.Group, filter func(node database.Node) bool) bool {
	var first database.Node
	for _, n := range group.Nodes {
		if filter(n) {
			first = n
			break
		}
	}

	return first == node
}
