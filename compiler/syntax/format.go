package syntax

import "strings"

// Format a source file.
func Format(source string) (string, *Error) {
	tokens, err := Tokenize("", source)
	if err != nil {
		return "", err
	}

	// The formatted code is stored here.
	var formatted strings.Builder

	// A deque allows us to perform lookahead without consuming tokens. This is
	// done when determining whether to reset the indentation of a line.
	deque := tokenDeque{tokens: tokens}

	// We track indentation within groups of parentheses, brackets and braces
	// using a stack. Whenever we encounted an opening grouping symbol, we push
	// `true` (indicating the symbol comes immediately before a new line) or
	// `false` (it doesn't) to the stack. Closing grouping symbols pop from the
	// stack.
	var groups []bool

	type LineIndent int

	const (
		LineIndentGroup LineIndent = iota
		LineIndentTrailingOperator
	)

	// Similarly, we track what caused a line to be indented. This is used to
	// reset the line indent only as far as needed. For example...
	//
	// ```wipple
	// {
	//   f :
	//     x ->
	//       42
	//
	//  f ()
	// }
	// ```
	//
	// ...would cause `LineIndentGroup` to be pushed once, and then
	// `LineIndentTrailingOperator` to be pushed twice (for the definition of
	// `f`). Once the definition of `f` is over, we want to remove the
	// `LineIndentTrailingOperator` indents, but not the `Group` indent, since
	// we want the call to `f` to remain indented within the block.
	//
	// You'll see this pattern several times below -- we remove
	// `LineIndentTrailingOperator` indents until we encounter a `Group` indent,
	// and then remove a single `Group` indent.
	var lineIndents []LineIndent

	// This tracks whether to insert a space before a token. We add padding
	// after non-grouping symbols like names and numbers, but skip padding after
	// parentheses, brackets and braces. Padding is also only respected for non-
	// grouping symbols; we want code like `( x )` to be formatted as `(x)`, not
	// `(x )`.
	pad := true

	// This tracks whether the previous token was an opening grouping symbol for
	// the purpose of inserting a space before a comment. Even though we don't
	// insert a space in the case of `(x`, we always insert a space before
	// comments (ie. `( -- comment`). When we insert a comment, we check if
	// either `pad` or `firstInGroup` are set.
	firstInGroup := false

	popTrailingOperators := func() {
		for len(lineIndents) > 0 && lineIndents[len(lineIndents)-1] == LineIndentTrailingOperator {
			lineIndents = lineIndents[:len(lineIndents)-1]
		}
	}

	popGroupIndent := func() {
		if len(lineIndents) > 0 && lineIndents[len(lineIndents)-1] == LineIndentGroup {
			lineIndents = lineIndents[:len(lineIndents)-1]
		}
	}

	popGroup := func() bool {
		if len(groups) == 0 {
			return false
		}
		indented := groups[len(groups)-1]
		groups = groups[:len(groups)-1]
		return indented
	}

	// Iterate through the tokens, writing them to `formatted` one at a time.
	for token := deque.advance(); token != nil; token = deque.advance() {
		if token.kind == "LeftParenthesis" {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteByte('(')

			pad = false
			firstInGroup = true

			if next := deque.peek(); next != nil && next.kind == "LineBreak" {
				lineIndents = append(lineIndents, LineIndentGroup)
				groups = append(groups, true)
			} else {
				groups = append(groups, false)
			}
		} else if token.kind == "RightParenthesis" {
			formatted.WriteByte(')')

			pad = true
			firstInGroup = false

			if indented := popGroup(); indented {
				popTrailingOperators()
				popGroupIndent()
			}
		} else if token.kind == "LeftBracket" {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteByte('[')

			pad = false
			firstInGroup = true

			if next := deque.peek(); next != nil && next.kind == "LineBreak" {
				lineIndents = append(lineIndents, LineIndentGroup)
				groups = append(groups, true)
			} else {
				groups = append(groups, false)
			}
		} else if token.kind == "RightBracket" {
			formatted.WriteByte(']')

			pad = true
			firstInGroup = false

			if indented := popGroup(); indented {
				popTrailingOperators()
				popGroupIndent()
			}
		} else if token.kind == "LeftBrace" {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteByte('{')

			pad = false
			firstInGroup = true

			if next := deque.peek(); next != nil && next.kind == "LineBreak" {
				lineIndents = append(lineIndents, LineIndentGroup)
				groups = append(groups, true)
			} else {
				groups = append(groups, false)
			}
		} else if token.kind == "RightBrace" {
			formatted.WriteByte('}')

			pad = true
			firstInGroup = false
			if indented := popGroup(); indented {
				popTrailingOperators()
				popGroupIndent()
			}
		} else if token.kind == "LineBreak" {
			// Collapse multiple line breaks into at most two line breaks.
			// For example:
			//
			// ```wipple
			// foo
			//
			//
			// bar
			// ```
			//
			// Is formatted as:
			//
			// ```wipple
			// foo
			//
			// bar
			// ```
			multiple := len(token.value) > 1

			// As long as we encounter closing grouping symbols before any
			// other tokens, we need to remove indentation. That way, code
			// like this:
			//
			// ```wipple
			// {
			//   x
			// }
			// ```
			//
			// Isn't formatted as:
			//
			// ```wipple
			// {
			//   x
			//   }
			// ```
			//
			// We store the closing symbols in a queue and write them to
			// `formatted` after removing indentation.
			var queue strings.Builder

			// This is used to insert leading indentation if the first token
			// is an operator.
			leadingIndent := false

			for {
				next := deque.peek()
				if next == nil {
					break
				}

				var c byte
				if next.kind == "RightParenthesis" {
					c = ')'
				} else if next.kind == "RightBracket" {
					c = ']'
				} else if next.kind == "RightBrace" {
					c = '}'
				} else if TokenIsOperator(next.kind) {
					// Only set the leading indent if the operator is
					// the first token on the line. Otherwise, code
					// like:
					//
					// ```wipple
					// {
					// } -> {}
					// ```
					//
					// Would insert a leading indent because of the
					// `->` and be formatted as:
					//
					// ```wipple
					// {
					//   } -> {}
					// ```
					leadingIndent = queue.Len() == 0
					c = 0
				} else {
					// Stop on any other token and so it can be printed
					// as normal.
					c = 0
				}

				if c == 0 {
					break
				}

				deque.advance()
				queue.WriteByte(c)

				// Make sure to handle the standard resetting of
				// grouping/indentation, since here, the most recent token
				// was a closing grouping symbol.
				if indented := popGroup(); indented {
					popTrailingOperators()
					popGroupIndent()
				}
			}

			// Add a single extra new line if there are multiple line
			// breaks.
			if multiple {
				formatted.WriteByte('\n')
			}

			indent := len(lineIndents)
			if leadingIndent {
				indent += 1
			}

			formatted.WriteByte('\n')
			for i := 0; i < indent; i++ {
				// Wipple uses 2-space indents.
				formatted.WriteString("  ")
			}

			formatted.WriteString(queue.String())

			pad = queue.Len() != 0
			firstInGroup = false

			// Fially, we determine whether to reset the trailing operator
			// indent. We do this by looking ahead to the end of the next
			// line. The rules for resetting indentation are:
			//
			//   -  If the last token (on the following line) is an opening
			//      grouping symbol or operator, don't reset. In fact, we
			//      indent another level.
			//
			//   -  Any other token (eg. a name) causes a reset because
			//      the operator expression is over.
			reset := queue.Len() != 0
			for _, t := range deque.remaining() {
				if t.kind == "LineBreak" {
					break
				}
				if TokenIsOpening(t.kind) || TokenIsOperator(t.kind) {
					reset = false
				} else {
					reset = true
				}
			}

			if reset {
				popTrailingOperators()
			}
		} else if token.kind == "Comment" {
			if pad || firstInGroup {
				formatted.WriteByte(' ')
			}

			formatted.WriteString("--")
			formatted.WriteString(token.value)
		} else if TokenIsKeyword(token.kind) {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteString(token.value)
			pad = true
			firstInGroup = false
		} else if TokenIsBinaryOperator(token.kind) {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteString(token.value)

			pad = true
			firstInGroup = false

			if next := deque.peek(); next != nil && next.kind == "LineBreak" {
				lineIndents = append(lineIndents, LineIndentTrailingOperator)
			}
		} else if TokenIsVariadicOperator(token.kind) {
			formatted.WriteString(token.value)

			pad = true
			firstInGroup = false

			if next := deque.peek(); next != nil && next.kind == "LineBreak" {
				lineIndents = append(lineIndents, LineIndentTrailingOperator)
			}
		} else if TokenIsNonAssociativeOperator(token.kind) {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteString(token.value)

			pad = true
			firstInGroup = false

			if next := deque.peek(); next != nil && next.kind == "LineBreak" {
				lineIndents = append(lineIndents, LineIndentTrailingOperator)
			}
		} else if token.kind == "CapitalName" || token.kind == "LowercaseName" {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteString(token.value)
			pad = true
			firstInGroup = false
		} else if token.kind == "String" {
			if pad {
				formatted.WriteByte(' ')
			}

			quote := byte('"')
			if strings.Contains(token.value, `"`) {
				quote = '\''
			}

			formatted.WriteByte(quote)
			formatted.WriteString(token.value)
			formatted.WriteByte(quote)
			pad = true
			firstInGroup = false
		} else if token.kind == "Number" {
			if pad {
				formatted.WriteByte(' ')
			}

			formatted.WriteString(token.value)

			pad = true
			firstInGroup = false
		} else {
			panic("unknown token kind: " + token.kind)
		}
	}

	return strings.TrimSpace(formatted.String()), nil
}

type tokenDeque struct {
	tokens []*Token
	index  int
}

func (d *tokenDeque) advance() *Token {
	if d.index >= len(d.tokens) {
		return nil
	}
	t := d.tokens[d.index]
	d.index++
	return t
}

func (d *tokenDeque) peek() *Token {
	if d.index >= len(d.tokens) {
		return nil
	}
	return d.tokens[d.index]
}

func (d *tokenDeque) remaining() []*Token {
	if d.index >= len(d.tokens) {
		return nil
	}
	return d.tokens[d.index:]
}
