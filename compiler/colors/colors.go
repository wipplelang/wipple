package colors

import (
	"fmt"

	"github.com/fatih/color"
)

func WithoutColor(f func()) {
	prev := color.NoColor
	color.NoColor = true
	f()
	color.NoColor = prev
}

func Code(s string) string {
	if color.NoColor {
		return fmt.Sprintf("`%s`", s)
	} else {
		return color.BlueString(s)
	}
}

func Conflict(s string) string {
	if color.NoColor {
		return s
	} else {
		return color.RedString(s)
	}
}

func Extra(s string) string {
	if color.NoColor {
		return fmt.Sprintf("(%s)", s)
	} else {
		return color.New(color.Faint).Sprintf("(%s)", s)
	}
}

func Title(s string) string {
	if color.NoColor {
		return s
	} else {
		return color.New(color.Bold, color.Underline).Sprintf("%s", s)
	}
}

func Underline(s string) string {
	if color.NoColor {
		return s
	} else {
		return color.New(color.Underline).Sprintf("%s", s)
	}
}
