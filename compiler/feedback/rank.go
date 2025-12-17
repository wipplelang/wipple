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
	RankBounds
	RankTypes
	RankPlaceholders
)

func sort(items []FeedbackItem) {
	slices.SortStableFunc(items, func(left FeedbackItem, right FeedbackItem) int {
		leftSpan := database.GetSpanFact(left.On)
		rightSpan := database.GetSpanFact(right.On)

		if leftSpan.Start.Line == rightSpan.Start.Line {
			return int(left.Rank) - int(right.Rank)
		}

		return leftSpan.Start.Line - rightSpan.Start.Line
	})
}
