package feedback

import (
	"bytes"
	"fmt"
	"regexp"
	"strings"

	"wipple/colors"
	"wipple/database"
	"wipple/queries"
	"wipple/typecheck"
	"wipple/visit"
)

type Render struct {
	db  *database.Db
	buf bytes.Buffer
}

func NewRender(db *database.Db) *Render {
	return &Render{db: db}
}

func (render *Render) WriteString(s string) {
	fmt.Fprintf(&render.buf, "%s", s)
}

func (render *Render) WriteBreak() {
	fmt.Fprintf(&render.buf, "\n\n")
}

func (render *Render) WriteNumber(n int, singular string, plural string) {
	if n == 1 {
		fmt.Fprintf(&render.buf, "%d %s", n, singular)
	} else {
		fmt.Fprintf(&render.buf, "%d %s", n, plural)
	}
}

func (render *Render) WriteOrdinal(n int) {
	suffix := "th"
	switch n % 10 {
	case 1:
		suffix = "st"
	case 2:
		suffix = "nd"
	case 3:
		suffix = "rd"
	}

	fmt.Fprintf(&render.buf, "%d%s", n, suffix)
}

func (render *Render) WriteNode(node database.Node) {
	fmt.Fprintf(&render.buf, "%s", database.RenderNode(node))
}

func (render *Render) WriteDefinition(node database.Node) {
	fmt.Fprintf(&render.buf, "%s", database.RenderDefinition(node))
}

func (render *Render) WriteCode(code string) {
	fmt.Fprintf(&render.buf, "%s", colors.Code(code))
}

func (render *Render) WriteType(ty typecheck.Type) {
	// Get the latest type
	if node, ok := ty.(database.Node); ok {
		if fact, ok := database.GetFact[typecheck.TypedFact](node); ok && fact.Group != nil && len(fact.Group.Types) > 0 {
			ty = fact.Group.Types[0]
		}
	}

	fmt.Fprintf(&render.buf, "%s", colors.Code(typecheck.DisplayType(ty, true)))
}

func (render *Render) WriteBound(bound typecheck.ResolvedBound) {
	fmt.Fprintf(&render.buf, "%s", colors.Code(typecheck.DisplayResolvedBound(bound)))
}

func (render *Render) WriteList(items []func(), separator string, limit int) {
	if len(items) > 2 {
		for i, item := range items {
			if limit > 0 && i >= limit {
				remaining := len(items) - limit

				var trailing string
				if remaining == 1 {
					trailing = "other"
				} else {
					trailing = "others"
				}

				fmt.Fprintf(&render.buf, ", %s %d %s", separator, remaining, trailing)
				break
			}

			if i > 0 && i == len(items)-1 {
				fmt.Fprintf(&render.buf, ", %s ", separator)
			} else if i > 0 {
				fmt.Fprintf(&render.buf, ", ")
			}

			item()
		}
	} else if len(items) == 2 {
		items[0]()
		fmt.Fprintf(&render.buf, " %s ", separator)
		items[1]()
	} else {
		items[0]()
	}
}

type Links = map[string]func(*Render)

func (render *Render) WriteComments(data queries.CommentsData) {
	links := Links{}
	for name, linksData := range data.Links {
		links[name] = func(render *Render) {
			render.WriteNode(linksData.Node)
		}

		links[name+"@related"] = func(render *Render) {
			related := make([]func(), 0, len(linksData.Related))
			for _, link := range linksData.Related {
				related = append(related, func() {
					render.WriteNode(link)
				})
			}

			render.WriteList(related, "and", 3)
		}

		links[name+"@type"] = func(render *Render) {
			types := make([]func(), 0, len(linksData.Types))
			for _, ty := range linksData.Types {
				types = append(types, func() {
					render.WriteType(ty)
				})
			}

			render.WriteList(types, "or", 3)
		}
	}

	commentsString := strings.TrimSpace(strings.Join(data.Comments, "\n"))
	matches := regexp.MustCompile("(?s)\\[`([^`]+)`\\]").FindAllStringIndex(commentsString, -1)

	lastIndex := 0
	for _, match := range matches {
		start := match[0]
		end := match[1]

		before := commentsString[lastIndex:start]

		key := commentsString[start:end]
		key = key[2 : len(key)-2] // remove brackets

		lastIndex = end

		render.WriteString(before)

		link, ok := links[key]
		if !ok {
			render.WriteCode("_")
			continue
		}

		link(render)
	}

	render.WriteString(commentsString[lastIndex:])
}

func (render *Render) WriteConstraint(prefix string, constraint typecheck.Constraint, seenLinks map[database.Node]struct{}) bool {
	node := constraint.Info().Node

	switch constraint := constraint.(type) {
	case *typecheck.GroupConstraint:
		if node == nil || database.IsHiddenNode(node) || database.LspEnabled {
			return false
		}

		if fact, ok := database.GetFact[visit.ResolvedFact](node); ok && len(fact.Definitions) == 1 {
			definition := fact.Definitions[0].GetNode()
			if _, ok := seenLinks[definition]; ok {
				return false
			} else {
				seenLinks[definition] = struct{}{}
			}

			render.WriteString(prefix)
			render.WriteString("See ")
			render.WriteDefinition(definition)
			render.WriteString(".")

			return true
		}
	case *typecheck.TypeConstraint:
		if node == nil || database.IsHiddenNode(node) || constraint.Type.Instantiate != nil {
			return false
		}

		span := database.GetSpanFact(node)

		// Don't repeat the type if it is from the source code
		if span.Source == typecheck.DisplayType(constraint.Type, true) {
			render.WriteString(prefix)
			render.WriteString("Explicitly annotated as a ")
			render.WriteType(constraint.Type)
			render.WriteString(".")
		} else {
			render.WriteString(prefix)
			render.WriteNode(node)
			render.WriteString(" is a ")
			render.WriteType(constraint.Type)
			render.WriteString(".")
		}

		return true
	case *typecheck.BoundConstraint:
		if constraint.Solver == nil {
			return false
		}

		if _, ok := seenLinks[node]; ok {
			return false
		} else {
			seenLinks[node] = struct{}{}
		}

		bound := typecheck.ResolvedBound{
			UnresolvedBound: constraint.Bound,
			Solver:          constraint.Solver,
		}

		render.WriteString(prefix)

		render.WriteNode(bound.Source)
		render.WriteString(" requires ")
		render.WriteBound(bound)
		render.WriteString(".")
	}

	return false
}

func (render *Render) Finish() string {
	return render.buf.String()
}
