package visit

import (
	"fmt"
	"reflect"
	"slices"

	"wipple/database"
	"wipple/typecheck"
)

type ResolvedFact struct {
	Name        string
	Definitions []Definition
}

func (fact ResolvedFact) String() string {
	if len(fact.Definitions) == 0 {
		return "unresolved"
	}

	s := fmt.Sprintf("resolved to %d definition(s): ", len(fact.Definitions))
	for i, definition := range fact.Definitions {
		if i > 0 {
			s += ", "
		}

		s += database.DisplayNode(definition.GetNode())
	}

	return s
}

type DefinitionConstraintsFact []typecheck.Constraint

func (fact DefinitionConstraintsFact) String() string {
	return "has definition constraints"
}

type TypeParametersFact []database.Node

func (fact TypeParametersFact) String() string {
	return "has type parameters"
}

type Visit interface {
	database.Node
	Visit(visitor *Visitor)
}

type Visitor struct {
	Db                *database.Db
	Scopes            []Scope
	CurrentDefinition *CurrentDefinition
	CurrentMatch      *CurrentMatch
	Queue             Queue

	constraints []typecheck.Constraint
}

type Result struct {
	Constraints []typecheck.Constraint
	Scope       Scope
}

func NewVisitor(db *database.Db, scopes []Scope) *Visitor {
	visitor := &Visitor{
		Db:     db,
		Scopes: scopes,
	}

	visitor.PushScope()

	return visitor
}

func (visitor *Visitor) Visit(node database.Node) {
	visitor.Db.Register(node)

	if visit, ok := node.(Visit); ok {
		visit.Visit(visitor)
	} else {
		panic(fmt.Sprintf("node is missing `Visit` method: %s", database.DisplayNode(node)))
	}
}

func (visitor *Visitor) Constraint(constraint typecheck.Constraint) {
	if visitor.CurrentDefinition != nil {
		visitor.CurrentDefinition.Constraint(constraint)
	} else {
		visitor.constraints = append(visitor.constraints, constraint)
	}
}

func (visitor *Visitor) PushScope() {
	visitor.Scopes = append(visitor.Scopes, Scope{})
}

func (visitor *Visitor) PeekScope() Scope {
	return visitor.Scopes[len(visitor.Scopes)-1]
}

func (visitor *Visitor) PopScope() Scope {
	scope := visitor.PeekScope()
	visitor.Scopes = visitor.Scopes[:len(visitor.Scopes)-1]
	return scope
}

func Resolve[T Definition](visitor *Visitor, name string, node database.Node) (T, bool) {
	if definition, ok := ResolveOf(visitor, name, node, []reflect.Type{reflect.TypeFor[T]()}); ok {
		return definition.(T), true
	}

	var zero T
	return zero, false
}

func ResolveOf(visitor *Visitor, name string, node database.Node, types []reflect.Type) (Definition, bool) {
	definitions, ok := PeekOf(visitor, name, types)

	database.SetFact(node, ResolvedFact{
		Name:        name,
		Definitions: definitions,
	})

	if !ok {
		return nil, false
	}

	return definitions[0], true
}

func Peek[T Definition](visitor *Visitor, name string) ([]T, bool) {
	if definitions, ok := PeekOf(visitor, name, []reflect.Type{reflect.TypeFor[T]()}); ok {
		result := make([]T, 0, len(definitions))
		for _, definition := range definitions {
			result = append(result, definition.(T))
		}

		return result, true
	}

	return nil, false
}

func PeekOf(visitor *Visitor, name string, types []reflect.Type) ([]Definition, bool) {
	for _, scope := range slices.Backward(visitor.Scopes) {
		if definitions, ok := scope[name]; ok {
			matching := make([]Definition, 0, len(definitions))
			for _, d := range definitions {
				if slices.Contains(types, reflect.TypeOf(d)) {
					matching = append(matching, d)
				}
			}

			return matching, len(matching) > 0
		}
	}

	return nil, false
}

func (visitor *Visitor) Define(name string, definition Definition) {
	scope := visitor.PeekScope()
	scope[name] = append(scope[name], definition)
}

func Defining[T Definition](visitor *Visitor, node database.Node, f func() (T, bool)) (T, bool) {
	existingDefinition := visitor.CurrentDefinition
	visitor.CurrentDefinition = &CurrentDefinition{Node: node}
	resultDefinition, ok := f()
	visitor.CurrentDefinition = existingDefinition

	if ok {
		database.SetFact(node, DefinedFact{Definition: resultDefinition})
	}

	return resultDefinition, ok
}

func Matching[T any](visitor *Visitor, temporary database.Node, allowSet bool, f func() T) T {
	existingMatch := visitor.CurrentMatch

	visitor.CurrentMatch = &CurrentMatch{
		Node:     temporary,
		AllowSet: allowSet,
	}

	result := f()

	visitor.CurrentMatch = existingMatch

	return result
}

func (visitor *Visitor) VisitMatching(pattern database.Node) database.Node {
	temporary := &database.HiddenNode{Facts: database.NewFacts(database.GetSpanFact(pattern))}
	visitor.Db.Register(temporary)

	Matching(visitor, temporary, false, func() struct{} {
		visitor.Visit(pattern)
		return struct{}{}
	})

	return temporary
}

func (visitor *Visitor) Finish() Result {
	for {
		entry, ok := visitor.Queue.dequeue()
		if !ok {
			break
		}

		previousScopes := slices.Clone(visitor.Scopes)
		previousDefinition := visitor.CurrentDefinition
		visitor.Scopes = entry.scopes
		visitor.CurrentDefinition = entry.currentDefinition

		entry.f()

		visitor.Scopes = previousScopes
		visitor.CurrentDefinition = previousDefinition
	}

	return Result{
		Constraints: visitor.constraints,
		Scope:       visitor.PopScope(),
	}
}

type Scope map[string][]Definition

type CurrentDefinition struct {
	Node                   database.Node
	ImplicitTypeParameters bool
	WithinConstantValue    bool
}

func (definition *CurrentDefinition) Constraints() []typecheck.Constraint {
	return GetDefinitionConstraints(definition.Node)
}

func GetDefinitionConstraints(node database.Node) []typecheck.Constraint {
	constraints, _ := database.GetFact[DefinitionConstraintsFact](node)
	return constraints
}

func (definition *CurrentDefinition) Constraint(constraint typecheck.Constraint) {
	if definition.WithinConstantValue {
		constraint.ShouldInstantiate = false
	}

	constraints := GetDefinitionConstraints(definition.Node)
	constraints = append(constraints, constraint)
	database.SetFact(definition.Node, DefinitionConstraintsFact(constraints))
}

func (definition *CurrentDefinition) WithImplicitTypeParameters(f func()) {
	definition.ImplicitTypeParameters = true
	f()
	definition.ImplicitTypeParameters = false
}

type CurrentMatch struct {
	Node     database.Node
	AllowSet bool
}

type queuedVisit struct {
	f                 func()
	scopes            []Scope
	currentDefinition *CurrentDefinition
}

// Needed so definitions are resolved before nodes that reference them
type Queue struct {
	afterTypeDefinitions []queuedVisit
	afterAllDefinitions  []queuedVisit
	afterAllExpressions  []queuedVisit
}

func (visitor *Visitor) AfterTypeDefinitions(f func()) {
	visitor.enqueue(&visitor.Queue.afterTypeDefinitions, f)
}

func (visitor *Visitor) AfterAllDefinitions(f func()) {
	visitor.enqueue(&visitor.Queue.afterAllDefinitions, f)
}

func (visitor *Visitor) AfterAllExpressions(f func()) {
	visitor.enqueue(&visitor.Queue.afterAllExpressions, f)
}

func (visitor *Visitor) enqueue(entry *[]queuedVisit, f func()) {
	*entry = append(*entry, queuedVisit{
		f:                 f,
		scopes:            slices.Clone(visitor.Scopes),
		currentDefinition: visitor.CurrentDefinition,
	})
}

func (queue *Queue) dequeue() (queuedVisit, bool) {
	entries := []*[]queuedVisit{
		&queue.afterTypeDefinitions,
		&queue.afterAllDefinitions,
		&queue.afterAllExpressions,
	}

	for _, entry := range entries {
		if len(*entry) > 0 {
			f := (*entry)[0]
			*entry = (*entry)[1:]
			return f, true
		}
	}

	return queuedVisit{}, false
}
