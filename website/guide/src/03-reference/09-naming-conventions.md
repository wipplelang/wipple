# Naming conventions

-   Names in Wipple use `kebab-case`.
-   Regular variables and constants are `lowercase`.
-   Types, enum variants, and traits are `Uppercase`.
-   Append `?` to `Boolean`s (eg. `valid?`), `Maybe`s and `Result`s (eg. `username?`), functions which return these values, and functions which conditionally execute their input (eg. `when?`).
-   Append `!` to functions which mutate state (eg. `increment!`) or return different outputs given the same input (eg. `get!`). External output is considered to have no effect on the program's state (eg. `show`, `drive-motor`).
-   Prefer punctuation for fundamental or abstract operators (eg. `:`, `|`), and prefer short English names for other operators (eg. `and`, `or`). Only use operators when there is a fundamental or natural relationship between the left- and right-hand side, otherwise prefer functions or templates.
-   Prefer full words over abbreviations (eg. `error` over `err` or `e`, `items` over `xs`), and prefer to name things after their actual function instead of after historical names (eg. `first` over `car`, `show` over `print`).