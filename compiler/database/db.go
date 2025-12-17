package database

import (
	"fmt"
	"io"
	"slices"
)

var LspEnabled = false

type Db struct {
	parent *Db
	nodes  []Node
}

func NewDb(parent *Db) *Db {
	return &Db{
		parent: parent,
		nodes:  []Node{},
	}
}

func (db *Db) Register(node Node) {
	db.nodes = append(db.nodes, node)
}

func ContainsNode(db *Db, f func(node Node) bool) bool {
	for current := db; current != nil; current = current.parent {
		if slices.ContainsFunc(current.nodes, f) {
			return true
		}
	}

	return false
}

func ContainsFact[T any, U any](db *Db, f func(node Node, fact T) (U, bool)) (U, bool) {
	for current := db; current != nil; current = current.parent {
		for _, node := range current.nodes {
			if fact, ok := GetFact[T](node); ok {
				if result, ok := f(node, fact); ok {
					return result, true
				}
			}
		}
	}

	var zero U
	return zero, false
}

func (db *Db) Write(w io.Writer, filter func(node Node) bool) {
	nodes := make([]Node, len(db.nodes))
	copy(nodes, db.nodes)
	slices.SortStableFunc(nodes, func(left Node, right Node) int {
		return CompareSpans(GetSpanFact(left), GetSpanFact(right))
	})

	for _, node := range nodes {
		if IsHiddenNode(node) || (filter != nil && !filter(node)) {
			continue
		}

		_, err := fmt.Fprintf(w, "%v\n%v\n", DisplayNode(node), node.GetFacts())
		if err != nil {
			panic(err)
		}
	}
}
