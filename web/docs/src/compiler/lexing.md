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

Lexing is a bit more complicated than `let tokens = code.split(" ")`, since not all tokens have whitespace between them — we want to split `(x)` into three tokens (`(`, `x`, and `)`), not one. Wipple uses the [Logos](https://github.com/maciejhirsz/logos) library to define [regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Regular_expressions) that denote boundaries between tokens. The lexer is created in [`wipple_syntax::tokenize::tokenize`](/api/wipple_syntax/tokenize/fn.tokenize.html).
