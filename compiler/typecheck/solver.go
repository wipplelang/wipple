package typecheck

import (
	"reflect"
	"slices"

	"wipple/database"
)

type Solver struct {
	Db               *database.Db
	Constraints      Constraints
	ImpliedInstances []Instance
	Progress         bool
	Error            bool
	groups           *groups
}

func NewSolver(db *database.Db) *Solver {
	return &Solver{
		Db:     db,
		groups: newGroups(nil),
	}
}

func CopySolver(other *Solver) *Solver {
	solver := NewSolver(other.Db)
	solver.Inherit(other)
	return solver
}

func (s *Solver) Inherit(other *Solver) {
	s.groups = newGroups(other.groups)
}

func (s *Solver) AppendGroup(group *Group) {
	s.groups.append(group, func(left, right *ConstructedType) bool {
		return s.unifyConstructedTypes(left, right)
	})
}

func (s *Solver) Run() bool {
	progress := false
	for {
		s.Progress = false
		s.RunPassUntil(nil)
		if !s.Progress {
			break
		}

		progress = true
	}

	// Run a final pass
	s.RunPassUntil(nil)

	return progress || s.Progress
}

func (s *Solver) RunPassUntil(stop reflect.Type) {
	s.Constraints.RunUntil(s, stop)

	if !s.Progress {
		s.Constraints.RunDefaults(s)
	}
}

func (s *Solver) Imply(instance Instance) {
	if !slices.ContainsFunc(s.ImpliedInstances, func(existing Instance) bool {
		return existing.Node == instance.Node
	}) {
		s.ImpliedInstances = append(s.ImpliedInstances, instance)
	}
}

func (s *Solver) Unify(trace Constraint, left Type, right Type) {
	if left == right {
		return
	}

	var originalLeftNode database.Node
	originalLeftNode, leftWasNode := left.(database.Node)

	var originalRightNode database.Node
	originalRightNode, rightWasNode := right.(database.Node)

	if leftWasNode && rightWasNode {
		s.merge(trace, originalLeftNode, originalRightNode)
		s.Progress = true
		return
	}

	left = s.applyShallow(left)
	right = s.applyShallow(right)

	leftNode, leftIsNode := left.(database.Node)
	rightNode, rightIsNode := right.(database.Node)

	if leftIsNode && rightIsNode {
		s.merge(trace, leftNode, rightNode)
		s.Progress = true
	} else if leftIsNode {
		s.insert(trace, leftNode, right)
		s.Progress = true
	} else if rightIsNode {
		s.insert(trace, rightNode, left)
		s.Progress = true
	} else {
		left := left.(*ConstructedType)
		right := right.(*ConstructedType)

		ok := s.unifyConstructedTypes(left, right, originalLeftNode, originalRightNode)
		if !ok {
			s.Error = true

			// Report conflicts on the original nodes
			if leftWasNode {
				s.insert(trace, originalLeftNode, right)
			}
			if rightWasNode {
				s.insert(trace, originalRightNode, left)
			}
		}
	}
}

func (s *Solver) unifyConstructedTypes(left *ConstructedType, right *ConstructedType, originalNodes ...database.Node) bool {
	// Type parameters are unique
	if left.Instantiate != right.Instantiate {
		s.Error = true
		return false
	}

	if left.Tag == right.Tag {
		for i := 0; i < len(left.Children) && i < len(right.Children); i++ {
			leftChild := left.Children[i]
			rightChild := right.Children[i]

			if TypeReferencesNode(leftChild, originalNodes...) || TypeReferencesNode(rightChild, originalNodes...) {
				// Recursive types
				continue
			}

			s.Unify(nil, leftChild, rightChild)
		}
	}

	ok := left.Tag == right.Tag && len(left.Children) == len(right.Children)

	if !ok {
		s.Error = true
	}

	return ok
}

func (s *Solver) Apply(ty Type) Type {
	return TraverseType(ty, func(ty Type) (Type, bool) {
		return s.applyShallow(ty), false
	})
}

func (s *Solver) applyShallow(ty Type) Type {
	if node, ok := ty.(database.Node); ok {
		_, group, ok := s.groups.FindGroup(node)
		if !ok || len(group.Types) == 0 {
			return ty
		}

		return group.Types[0]
	}

	return ty
}

func (s *Solver) insert(trace Constraint, node database.Node, types ...Type) {
	if entry, group, ok := s.groups.FindGroup(node); ok {
		for _, ty := range types {
			switch ty := ty.(type) {
			case database.Node:
				s.merge(trace, node, ty)
				entry, group, _ = s.groups.FindGroup(node)
			case *ConstructedType:
				group = entry.Clone(group)
				group.Types = append(group.Types, ty)

				if trace != nil {
					group.Trace = append(group.Trace, trace)
				}
			}
		}

		group.normalize()

		return
	}

	groupNodes := []database.Node{node}
	groupTypes := make([]*ConstructedType, 0, 1)
	for _, ty := range types {
		switch ty := ty.(type) {
		case database.Node:
			groupNodes = append(groupNodes, ty)
		case *ConstructedType:
			groupTypes = append(groupTypes, ty)
		}
	}

	group := makeGroup(groupNodes, groupTypes, []Constraint{trace})
	group.normalize()

	s.AppendGroup(group)
}

func (s *Solver) merge(trace Constraint, leftNode database.Node, rightNode database.Node) {
	s.groups.merge(trace, leftNode, rightNode, func(left, right *ConstructedType) bool {
		return s.unifyConstructedTypes(left, right, leftNode, rightNode)
	})
}

func (s *Solver) Groups(order func(node database.Node) int) []*Group {
	var groups []*Group
	s.groups.Each(func(group *Group) {
		for i, ty := range group.Types {
			group.Types[i] = s.Apply(ty).(*ConstructedType)
		}

		group.normalize()

		slices.SortStableFunc(group.Nodes, func(left database.Node, right database.Node) int {
			return order(left) - order(right)
		})

		groups = append(groups, group)
	})

	return groups
}
