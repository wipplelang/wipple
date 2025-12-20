package feedback

import (
	"slices"
	"wipple/database"
)

type Rank int

const (
	RankSyntax Rank = iota
	RankNames
	RankCustom
	RankConflicts
	RankBounds
	RankUnknown
	RankPlaceholders
)

func sort(items []FeedbackItem) {
	slices.SortStableFunc(items, func(left FeedbackItem, right FeedbackItem) int {
		leftSpan := database.GetSpanFact(left.On[0])
		rightSpan := database.GetSpanFact(right.On[0])

		if leftSpan.Start.Line != rightSpan.Start.Line {
			return leftSpan.Start.Line - rightSpan.Start.Line
		}

		return int(left.Rank) - int(right.Rank)
	})
}
