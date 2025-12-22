package typecheck

import (
	"fmt"
	"slices"

	"wipple/database"
)

type Group struct {
	Nodes []database.Node
	Types []*ConstructedType
	Trace []Constraint
}

func makeGroup(nodes []database.Node, types []*ConstructedType, trace []Constraint) *Group {
	return &Group{
		Nodes: nodes,
		Types: types,
		Trace: trace,
	}
}

func NewGroup(nodes ...database.Node) *Group {
	return makeGroup(nodes, make([]*ConstructedType, 0, 1), nil)
}

func (group *Group) Clone() *Group {
	return &Group{
		Nodes: slices.Clone(group.Nodes),
		Types: slices.Clone(group.Types),
		Trace: slices.Clone(group.Trace),
	}
}

func (group *Group) unifyWithTypes(types []*ConstructedType, unify func(left *ConstructedType, right *ConstructedType) bool) {
	for _, ty := range types {
		if len(group.Types) == 0 || !unify(group.Types[0], ty) {
			// If the type cannot be unified, add it separately
			group.Types = append(group.Types, ty)
		}
	}

	group.normalize()
}

func (group *Group) normalize() {
	types := make([]*ConstructedType, 0, len(group.Types))
	for _, ty := range group.Types {
		if !slices.ContainsFunc(types, func(existing *ConstructedType) bool {
			return TypesAreEqual(ty, existing)
		}) {
			types = append(types, ty)
		}
	}

	if len(types) != len(group.Types) {
		group.Types = types
	}

	slices.SortStableFunc(group.Trace, func(left Constraint, right Constraint) int {
		leftNode := left.Info().Node
		rightNode := right.Info().Node

		if leftNode == nil || rightNode == nil {
			return 0
		}

		return database.CompareSpans(database.GetSpanFact(leftNode), database.GetSpanFact(rightNode))
	})
}

func (group *Group) String() string {
	nodes := ""
	i := 0
	for _, node := range group.Nodes {
		if i > 0 {
			nodes += ", "
		}

		nodes += database.DisplayNode(node)

		i++
	}

	types := ""
	for i, ty := range group.Types {
		if i > 0 {
			types += ", "
		}

		types += DisplayType(ty, true)
	}

	return fmt.Sprintf("Group{Nodes: [%s], Types: [%s]}", nodes, types)
}

type groups struct {
	parent  *groups
	entries map[database.Node]*Group
}

func newGroups(parent *groups) *groups {
	return &groups{
		parent:  parent,
		entries: map[database.Node]*Group{},
	}
}

func (g *groups) Each(f func(*Group)) {
	visited := map[*Group]struct{}{}
	for entry := g; entry != nil; entry = entry.parent {
		for _, group := range entry.entries {
			if _, ok := visited[group]; !ok {
				visited[group] = struct{}{}
				f(group)
			}
		}
	}
}

func (g *groups) append(group *Group, unify func(left *ConstructedType, right *ConstructedType) bool) {
	add := false
	groupsToUnify := map[*Group]struct{}{}
	for _, node := range group.Nodes {
		if _, group, ok := g.FindGroup(node); ok {
			groupsToUnify[group] = struct{}{}
		} else {
			add = true
		}
	}

	if add {
		g.add(group)
	}

	for existing := range groupsToUnify {
		existing.unifyWithTypes(group.Types, unify)
	}
}

func (g *groups) add(group *Group) {
	// Note that these changes will not propagate to the parent, which is OK
	// because the solver can `Inherit` this `groups` value.
	if group != nil {
		for _, node := range group.Nodes {
			g.entries[node] = group
		}
	}
}

func (g *groups) Clone(group *Group) *Group {
	newGroup := group.Clone()
	g.add(newGroup)
	return newGroup
}

func (g *groups) merge(trace Constraint, left database.Node, right database.Node, unify func(left *ConstructedType, right *ConstructedType) bool) {
	leftEntry, leftGroup, ok := g.FindGroup(left)
	if !ok {
		leftGroup = NewGroup(left)
		g.add(leftGroup)
	}

	_, rightGroup, ok := g.FindGroup(right)
	if !ok {
		rightGroup = NewGroup(right)
		g.add(rightGroup)
	}

	if leftGroup == rightGroup {
		return
	}

	oldGroup := rightGroup

	// Make a copy of the left group if it is shared with a parent
	newGroup := leftGroup
	if leftEntry != g {
		newGroup = g.Clone(leftGroup)
	}

	// Update the nodes to point to the new group. Note that these changes will
	// not propagate to the parent, which is OK because the solver can `Inherit`
	// this `groups` value.
	for _, node := range oldGroup.Nodes {
		g.entries[node] = newGroup
	}

	// Then merge the old group's contents into the new group.
	newGroup.Nodes = append(newGroup.Nodes, oldGroup.Nodes...)
	newGroup.unifyWithTypes(oldGroup.Types, unify)
	newGroup.Trace = append(newGroup.Trace, oldGroup.Trace...)

	if trace != nil {
		newGroup.Trace = append(newGroup.Trace, trace)
	}
}

func (g *groups) FindGroup(node database.Node) (*groups, *Group, bool) {
	for entry := g; entry != nil; entry = entry.parent {
		if group, ok := entry.entries[node]; ok {
			return entry, group, true
		}
	}

	return g, nil, false
}
