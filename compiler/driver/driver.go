package driver

import (
	"fmt"
	"io"
	"slices"
	"strings"

	"wipple/colors"
	"wipple/database"
	"wipple/feedback"
	"wipple/nodes/constraints"
	"wipple/nodes/file"
	"wipple/queries"
	"wipple/typecheck"
	"wipple/visit"

	"github.com/charmbracelet/x/ansi"
)

type RootNode struct {
	Files []*file.FileNode
	Facts *database.Facts
}

func init() {
	database.HideNode[*RootNode]()
}

func (node *RootNode) GetFacts() *database.Facts {
	return node.Facts
}

func (node *RootNode) Visit(visitor *visit.Visitor) {
	for _, file := range node.Files {
		visitor.Visit(file)
	}
}

func CloneRoot(root *RootNode) *RootNode {
	return &RootNode{
		Facts: database.CloneFacts(root.Facts),
	}
}

type TopLevelScopes []visit.Scope

func (fact TopLevelScopes) String() string {
	return "has top-level scopes"
}

func NewRoot(db *database.Db) *RootNode {
	root := &RootNode{Facts: database.EmptyFacts()}
	db.Register(root)
	return root
}

func MakeRoot() (*database.Db, *RootNode) {
	db := database.NewDb(nil)
	root := NewRoot(db)
	return db, root
}

func Compile(db *database.Db, root *RootNode, files []*file.FileNode) {
	root.Files = append(root.Files, files...)

	nodeIsFromFiles := func(node database.Node) bool {
		var span database.Span
		if fact, ok := database.GetFact[typecheck.InstantiatedFact](node); ok {
			span = database.GetSpanFact(fact.Source)
		} else {
			span = database.GetSpanFact(node)
		}

		return slices.ContainsFunc(files, func(file *file.FileNode) bool {
			return span.Path == database.GetSpanFact(file).Path
		})
	}

	// Define/resolve names and collect constraints

	topLevelScopes, _ := database.GetFact[TopLevelScopes](root)

	visitor := visit.NewVisitor(db, topLevelScopes)
	for _, file := range files {
		visitor.Visit(file)
	}

	topLevel := visitor.Finish()

	topLevelScopes = append(topLevelScopes, topLevel.Scope)
	database.SetFact(root, topLevelScopes)

	// Solve constraints from each definition, implying all bounds

	database.ContainsFact(db, func(definitionNode database.Node, definitionConstraints visit.DefinitionConstraintsFact) (struct{}, bool) {
		if !nodeIsFromFiles(definitionNode) {
			return struct{}{}, false
		}

		solver := typecheck.NewSolver(db)

		if instance, ok := database.ContainsFact(db, func(node database.Node, instances typecheck.InstancesFact) (typecheck.Instance, bool) {
			for _, instance := range instances {
				if instance.Node == definitionNode {
					return instance, true
				}
			}

			return typecheck.Instance{}, false
		}); ok {
			solver.Imply(instance)
		}

		// Also imply all of the definition's bounds (so they remain generic
		// while resolving the definition)
		for _, constraint := range definitionConstraints {
			if constraint.Info().Instance != nil {
				// Only imply bounds from constraints, not from inside the
				// definition's value!
				if _, ok := constraint.Info().Node.(*constraints.BoundConstraintNode); ok {
					solver.Imply(*constraint.Info().Instance)
				}
			}
		}

		solver.Constraints.Add(definitionConstraints...)
		solver.Run()

		setGroups(solver, nodeIsFromFiles)

		return struct{}{}, false
	})

	// Solve constraints from top-level expressions

	solver := typecheck.NewSolver(db) // definition constraints will be retrieved from `db` as needed
	database.ContainsFact(db, func(node database.Node, fact typecheck.TypedFact) (struct{}, bool) {
		if fact.Group != nil {
			solver.AppendGroup(fact.Group)
		}

		return struct{}{}, false
	})

	solver.Constraints.Add(topLevel.Constraints...)
	solver.Run()

	setGroups(solver, nodeIsFromFiles)

	// Check for overlapping instances

	database.ContainsFact(db, func(traitDefinition database.Node, instances typecheck.InstancesFact) (struct{}, bool) {
		visit.CheckForOverlappingInstances(db, traitDefinition, instances)
		return struct{}{}, false
	})

	// Resolve `Mismatched` trait for mismatched types

	if mismatchedTrait, ok := topLevel.Utilities["Mismatched"].(*visit.TraitDefinition); ok {
		for _, solver := range visit.RunMismatchedTrait(db, mismatchedTrait, nodeIsFromFiles) {
			setGroups(solver, nodeIsFromFiles)
		}
	}
}

func setGroups(solver *typecheck.Solver, filter func(node database.Node) bool) {
	groups := solver.Groups(queries.OrderGroupNodes)

	for _, group := range groups {
		var nodes []database.Node
		for _, node := range group.Nodes {
			if !filter(node) {
				continue
			}

			if existing, ok := database.GetFact[typecheck.TypedFact](node); ok && existing.Group == nil {
				database.SetFact(node, typecheck.TypedFact{Group: group})
				nodes = append(nodes, node)
			}
		}

		if len(nodes) > 0 {
			labels := make([]string, len(group.Types))
			for i, ty := range group.Types {
				labels[i] = typecheck.DisplayType(ty, true)
			}

			if len(labels) == 0 {
				labels = append(labels, "_")
			}

			solver.Db.Graph.Group(nodes, labels)
		}
	}
}

func WriteFeedback(db *database.Db, filter func(node database.Node) bool, filterFeedback []string, w io.Writer) int {
	seenFeedback := map[database.Node][]string{}
	feedbackCount := 0
	items := feedback.Collect(db, filter, func(item feedback.FeedbackItem) bool {
		if len(filterFeedback) > 0 && !slices.Contains(filterFeedback, item.Id) {
			return false
		}

		if database.IsHiddenNode(item.On[0]) || !filter(item.On[0]) {
			return false
		}

		if slices.Contains(seenFeedback[item.On[0]], item.Id) {
			return false
		}

		seenFeedback[item.On[0]] = append(seenFeedback[item.On[0]], item.Id)

		return true
	})

	for _, item := range items {
		indent := "  "

		message, _ := item.String()
		rendered := ansi.Wordwrap(message, 100-len(indent), " ")
		for i, line := range strings.Split(rendered, "\n") {
			if i > 0 {
				rendered += "\n" + indent
			} else {
				rendered = indent
			}

			rendered += line
		}

		if feedbackCount == 0 {
			_, err := fmt.Fprintf(w, "\n%s\n\n", colors.Title("Feedback:"))
			if err != nil {
				panic(err)
			}
		}

		_, err := fmt.Fprintf(w, "%s (%s):\n\n%s\n\n", database.RenderNode(item.On[0]), item.Id, rendered)
		if err != nil {
			panic(err)
		}

		feedbackCount++
	}

	return feedbackCount
}
