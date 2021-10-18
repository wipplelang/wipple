# Syntax

Wipple has a minimal syntax fundamentally inspired by Lisp. Wipple code consists of seven kinds of expressions:

-   **Names** (`x`, `do-something!`, `->`) are used to identify variables. When quoted, they represent data similar to text.
-   **Numbers** (`42`, `3.14`, `-1`) are used for calculations and are stored in decimal format.
-   **Text** (`"Hello, world"`, `"😀"`) is used to represent human-readable data and is stored in Unicode format.
-   **Lists** (`(a b c)`) are used to group expressions together. Inside blocks, the parentheses are inferred.
-   **Attributes** (`[a b c]`) are used to apply additional information to an expression at compile-time.
-   **Blocks** (`{ a b c }`) are used to execute a series of lists in order. Source files are inferred as blocks.
-   **Quoted forms** (`'a`, `'(a b c)`, `''(a b c)`) are used to insert expressions into the structure of another expression, or to represent code as data.

Comments begin with `--` and are ignored.

For example, this source file consists of the following expressions:

<table>
<thead>
<tr>
<td>Source code</td>
<td>Expression tree</td>
</tr>
</thead>
<tbody>
<tr>
<td>

```wipple
[doc "A person named bob"]
bob : Person "Bob"

test {
    -- Ensure math works
    assert (2 + 2 = 4)
}
```

</td>
<td>

```wipple
Block
  List (attributes: doc "A person named bob")
    Name "bob"
    Name ":"
    Name "Person"
    Text "Bob"
  List
    Name "test"
    Block
      List
        Name "assert"
        List
          Number 2
          Name "+"
          Number 2
          Name "="
          Number 4
```

</td>
</tr>
</tbody>

</table>

## A note on lists

Lists may span multiple lines, but the way they are parsed depends on whether they belong to a block or are between parentheses.

Inside a block, you can use indentation to merge multiple lines into a single list:

```wipple
-- Parsed as two lists
a b c
d e f

-- Parsed as one list
a b c
    d e f
```

Between parentheses, you can use any indentation you wish — all expressions belong to the list until the ending `)`.

## How do operators work if they aren't part of the syntax?

Lists are evaluated based on the operators defined in the code. You can even define your own operators to change how lists are evaluated! (TODO: Section on defining operators)
