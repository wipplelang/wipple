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

const MaxRank Rank = RankPlaceholders

func sort(items []FeedbackItem) []FeedbackItem {
	// Reduce noise by preferring lower-rank items
	minRank := MaxRank
	for _, item := range items {
		if item.Rank < minRank {
			minRank = item.Rank
		}
	}
	items = slices.DeleteFunc(items, func(item FeedbackItem) bool {
		return item.Rank > minRank
	})

	slices.SortStableFunc(items, func(left FeedbackItem, right FeedbackItem) int {
		leftSpan := database.GetSpanFact(left.On[0])
		rightSpan := database.GetSpanFact(right.On[0])

		return leftSpan.Start.Line - rightSpan.Start.Line
	})

	return items
}
