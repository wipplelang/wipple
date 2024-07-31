# Lexing

Lexing, aka. tokenization, is the process of splitting the string of source code into a list of "tokens". A good way to think of tokens is like they are atoms, the smallest meaningful unit of syntax.

This preprocessing step means the [parser](./parsing.md) doesn't have to iterate over the code character by character — without tokenization, there would be a lot of code to group characters together mixed with the parsing code. For example, without tokenization, the parser would have to do something like this:

> Input: `foo :: Number`
>
> -   Scan `f`; `f` is a character, so start a name.
> -   Scan `o`; `o` is a character and we have a name, so the name is `fo`.
> -   Scan `o`; `o` is a character and we have a name, so the name is `foo`.
> -   Scan whitespace; stop the name.
> -   Scan `:`; we either have `:` or `::`.
> -   Scan `:`; we have `::`.
> -   Scan whitespace; ignore.
> -   Scan `N`; `N` is a character, so start a name.
> -   etc.

But with lexing, the parser can already start with `foo`, `::`, and `Number` split into separate groups.

Lexing is a bit more complicated than `let tokens = code.split(" ")`, since not all tokens have whitespace between them — we want to split `(x)` into three tokens (`(`, `x`, and `)`), not one. Wipple uses the [Logos](https://github.com/maciejhirsz/logos) library to define [regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Regular_expressions) that denote boundaries between tokens.

## How Wipple tokenizes code

Open `compiler/syntax/src/tokenize.rs` to follow along.

1.  The source code string, `s`, is passed to the [`tokenize`](/api/wipple_syntax/tokenize/fn.tokenize.html) function.

2.  Wipple calls `logos::Lexer::new(s).spanned()`, which returns an [`Iterator`](https://doc.rust-lang.org/stable/std/iter/index.html) that produces a `Result<RawToken, ()>` and a `Range<usize>`. The `Result` is either `Ok(RawToken)` (a valid token) or `Err(())` (an invalid token — the default error type in Logos is `()`; Wipple currently does not have custom error handling for invalid tokens). The `Range` is called a "span", aka. the start and end positions in the source code where the token is located.

3.  Wipple [`map`](https://doc.rust-lang.org/stable/std/iter/trait.Iterator.html#method.map)s over each of these results, converting the [`RawToken`](/api/wipple_syntax/tokenize/enum.RawToken.html)s into [`Token`](/api/wipple_syntax/tokenize/enum.Token.html)s, which are categorized so that operations on similar kinds of tokens (eg. operators or keywords) can be shared. Any `Err(())` values are converted to [`Diagnostic::InvalidToken`](/api/wipple_syntax/tokenize/enum.Diagnostic.html#variant.InvalidToken). Both the `Ok` and `Err` variants are put in a [`WithInfo`](/api/wipple_util/struct.WithInfo.html) structure containing a [`Location`](/api/wipple_syntax/struct.Location.html) value that wraps the span.

4.  Wipple reports diagnostics for the `Err` values.

5.  Wipple calls [`to_logical_lines`](/api/wipple_syntax/tokenize/fn.to_logical_lines.html) on the `Ok` values, which replaces multiple line breaks with single line breaks, and removes line breaks between lines involving operators.

    For example:

    | Code              | Equivalent to | Behavior                                                       |
    | ----------------- | ------------- | -------------------------------------------------------------- |
    | `a b`             | `a b`         | Single line                                                    |
    | `a` <br> `b`      | `a` <br> `b`  | Line break                                                     |
    | `a` <br> <br> `b` | `a` <br> `b`  | Multiple line breaks are reduced <br> into a single line break |
    | `a` <br> `+ b`    | `a + b`       | Operator joins lines                                           |

6.  Wipple takes the processed list of tokens returned by [`to_logical_lines`](/api/wipple_syntax/tokenize/fn.to_logical_lines.html) and passes it to [`TokenTree::from_top_level`](/api/wipple_syntax/tokenize/enum.TokenTree.html#method.from_top_level). A "token tree" is a kind of concrete syntax tree that does grouping by parentheses and operators, but has no further syntactic structure. In this stage, comments are removed and the original code is no longer recoverable.

    [`TokenTree::from_top_level`](/api/wipple_syntax/tokenize/enum.TokenTree.html#method.from_top_level) works by maintaining a stack of groups — when a left parentheses is encountered, for example, a [`TokenTree::List`](/api/wipple_syntax/tokenize/enum.TokenTree.html) is pushed onto the stack, and when a right parentheses is encountered, this list is popped and appended to the group that's now on the top. Non-grouping symbols are always appended to the group on the top of the stack. We easily detect mismatched parentheses/brackets and braces by checking if the top of the stack exists and is the expected group.

    There's also a similar function, [`TokenTree::from_inline`](/api/wipple_syntax/tokenize/enum.TokenTree.html#method.from_inline), that just calls [`TokenTree::from_top_level`](/api/wipple_syntax/tokenize/enum.TokenTree.html#method.from_top_level) and extracts the first statement in the output. This will eventually be useful for tests, since you can pass a string like `1 + 2` and get back the operator expression directly instead of it being wrapped in a block.

7.  The resulting token tree is returned to the driver, and any diagnostics reported.

## Operators

If you look through the lexer, you'll see three kinds of operators: binary, non-associative, and variadic. Each one is processed a bit differently.

First, it's important to know that every operator has a **precedence**, or priority — operators with a higher precedence group more tightly. For example, `*` has a higher precedence than `+`, so `1 + 2 * 3` is equivalent to `1 + (2 * 3)` and not `(1 + 2) * 3`.

-   **Binary operators** accept two token trees, one on either side of the operator. Every binary operator has an **associativity**, which determines the grouping behavior when multiple operators of the same precedence are encountered.

    For example, `-` is left-associative (ie. has [`Associativity::Left`](/api/wipple_syntax/tokenize/enum.Associativity.html)), so `3 - 2 - 1` is equivalent to `(3 - 2) - 1` (`0`) and not `3 - (2 - 1)` (`2`).

    `->`, on the other hand, is right-associative, so `Number -> Number -> Number` is equivalent to `Number -> (Number -> Number)` (which returns a function as output) and not `(Number -> Number) -> Number` (which accepts a function as input).

-   **Non-associative operators** also accept two token trees, one on either side, but have no associativity. If Wipple encounters multiple non-associative operators with the same precedence, it will produce an error. Non-associativity is essentially a way to require that an operator is used only once in an expression. For example, `:` is non-associative because you can only have one definition per line. (`a : b : c` doesn't make sense in Wipple.)

-   **Variadic operators** accept zero or more token trees as input; the operator serves as a separator between each token tree. Wipple has two variadic operators: `;` for tuples and `,` for collections. You may provide a trailing operator after the inputs (ie. `1 , 2 , 3 ,` is valid). To represent zero inputs, specify the operator on its own: `,`. Normally, you wrap the operator in parentheses to avoid ambiguity: `(,)` is the typical syntax for an empty list.
