package queries

import (
	"wipple/database"
	"wipple/nodes/types"
	"wipple/typecheck"
	"wipple/visit"
)

func HighlightType(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if _, ok := database.GetFact[types.IsTypeFact](node); ok {
		if _, ok := node.(*types.TypeParameterNode); !ok {
			f(struct{}{})
		}
	}
}

func HighlightTrait(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	if resolved, ok := database.GetFact[visit.ResolvedFact](node); ok {
		for _, definition := range resolved.Definitions {
			if _, ok := definition.(*visit.TraitDefinition); ok {
				f(struct{}{})
				return
			}
		}
	}

	if defined, ok := database.GetFact[visit.DefinedFact](node); ok {
		if _, ok := defined.Definition.(*visit.TraitDefinition); ok {
			f(struct{}{})
		}
	}
}

func HighlightFunction(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	highlightTag(db, node, "function", f)
}

func HighlightTypeParameter(db *database.Db, node database.Node, filter func(node database.Node) bool, f func(struct{})) {
	highlightTag(db, node, "parameter", f)
}

func highlightTag(db *database.Db, node database.Node, tag any, f func(struct{})) {
	if _, ok := database.GetFact[visit.ResolvedFact](node); !ok {
		return
	}

	if _, ok := database.GetFact[visit.DefinedFact](node); ok {
		return
	}

	typed, ok := database.GetFact[typecheck.TypedFact](node)
	if !ok || typed.Group == nil || len(typed.Group.Types) == 0 {
		return
	}

	if typed.Group.Types[0].Tag == tag {
		f(struct{}{})
	}
}
