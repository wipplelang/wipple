package database

import (
	"bytes"
	"encoding/json"
	"fmt"
	"slices"
)

type Graph struct {
	initialNodes []Node
	Edges        []Edge
	Groups       []*Group
	Replacements map[Node]Node
}

type Edge struct {
	From  Node
	To    Node
	Label string
}

type Group struct {
	Nodes  []Node
	Labels []string
}

func NewGraph() *Graph {
	return &Graph{
		Edges:        []Edge{},
		Groups:       []*Group{},
		Replacements: map[Node]Node{},
	}
}

func (g *Graph) SetInitialNodes(nodes []Node) {
	g.initialNodes = nodes
}

func (g *Graph) Replace(node Node, replacement Node) {
	g.Replacements[node] = replacement
}

func (g *Graph) Edge(from Node, to Node, label string) {
	g.Edges = append(g.Edges, Edge{From: from, To: to, Label: label})
}

func (g *Graph) Group(nodes []Node, labels []string) {
	g.Groups = append(g.Groups, &Group{Nodes: nodes, Labels: labels})
}

func (g *Graph) HasNode(node Node) bool {
	for _, edge := range g.Edges {
		if edge.From == node || edge.To == node {
			return true
		}
	}

	for _, group := range g.Groups {
		if slices.Contains(group.Nodes, node) {
			return true
		}
	}

	return false
}

func (g *Graph) MarshalJSON() ([]byte, error) {
	type data struct {
		Nodes  []map[string]any `json:"nodes"`
		Groups []map[string]any `json:"groups"`
		Edges  []map[string]any `json:"edges"`
	}

	result := data{
		Nodes:  []map[string]any{},
		Groups: []map[string]any{},
		Edges:  []map[string]any{},
	}

	ids := map[Node]string{}
	id := func(node Node) string {
		id, ok := ids[node]
		if !ok {
			id = fmt.Sprintf("node%d", len(ids))
			ids[node] = id
		}

		return id
	}

	replace := func(node Node) Node {
		for {
			replacement, ok := g.Replacements[node]
			if !ok {
				break
			}

			delete(ids, node)
			node = replacement
		}

		return node
	}

	// Register the initial nodes
	for _, node := range g.initialNodes {
		id(replace(node))
	}

	edgeNodes := map[Node]map[Node]string{}
	for _, edge := range g.Edges {
		from := replace(edge.From)
		to := replace(edge.To)

		// Don't add edges between hidden and non-hidden nodes
		if IsHiddenNode(from) && !IsHiddenNode(to) {
			continue
		}

		// Register the nodes
		id(from)
		id(to)

		if _, ok := edgeNodes[from]; !ok {
			edgeNodes[from] = map[Node]string{}
		}

		edgeNodes[from][to] = edge.Label
	}

	groups := map[Node]*Group{}
	for _, group := range g.Groups {
		nodes := []string{}
		for _, node := range group.Nodes {
			replacement := replace(node)

			id, ok := ids[replacement]
			if !ok {
				continue
			}

			if !slices.Contains(nodes, id) {
				nodes = append(nodes, id)
			}

			delete(groups, node)
			groups[replacement] = group
		}
	}

	// Only keep edges that have nodes in a group
	for from, tos := range edgeNodes {
		_, hasFrom := groups[from]

		for to, label := range tos {
			_, hasTo := groups[to]

			if hasFrom && hasTo {
				result.Edges = append(result.Edges, map[string]any{
					"from":  id(from),
					"to":    id(to),
					"label": label,
				})
			}
		}
	}

	for node := range ids {
		if _, ok := groups[node]; !ok {
			delete(ids, node)
		}
	}

	g.Groups = slices.DeleteFunc(g.Groups, func(group *Group) bool {
		group.Nodes = slices.DeleteFunc(group.Nodes, func(node Node) bool {
			_, ok := groups[node]
			return !ok
		})

		return len(group.Nodes) == 0
	})

	for node, id := range ids {
		span := GetSpanFact(node)
		span.Source = NodeSource(span.Source)

		result.Nodes = append(result.Nodes, map[string]any{
			"id":   id,
			"span": span,
		})
	}

	for _, group := range g.Groups {
		var nodes []string
		for _, node := range group.Nodes {
			if id, ok := ids[node]; ok {
				nodes = append(nodes, id)
			}
		}

		if len(nodes) > 0 {
			result.Groups = append(result.Groups, map[string]any{
				"nodes":  nodes,
				"labels": group.Labels,
			})
		}
	}

	var buf bytes.Buffer
	encoder := json.NewEncoder(&buf)
	encoder.SetEscapeHTML(false)
	err := encoder.Encode(result)
	if err != nil {
		return nil, err
	}

	return buf.Bytes(), nil
}
