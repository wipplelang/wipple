# Language basics

## Comments

Comments are declared using two dashes (`--`) and continue until the end of the line.

```wipple
-- This is a comment

show "Hello, world!" -- this code is executed...
-- show "Hello, world!" ...but this is not
```

## Text and numbers

You can write text (aka. strings) using double quotes:

```wipple
"Lorem ipsum dolor sit amet, consectetur adipiscing elit."
```

Numbers are written as you'd expect:

```wipple
42
3.14
0.5
```

When writing a decimal number like `.1`, you have to put the `0` in front:

```wipple
.1 -- interpreted as name
0.1 -- interpreted as number
```

Wipple stores numbers in base 10, just like how they are written in source.

```wipple
-- In other languages this would return 0.30000000000000004, in Wipple you get
-- 0.3 as expected
0.1 + 0.2
```

> **Tip:** You can do simple math using `+`, `-`, `*` (for multiplication) and `/` (for division).

## Variables

You can declare variables using a colon (`:`, pronounced _is_), like this:

```wipple
meaning-of-life : 42

show meaning-of-life -- shows 42 on the right
```

You aren't just limited to numbers, though — any piece of Wipple code can be assigned to a variable:

```wipple
one : 1
two : 2
three : one + two
```

By convention, if a variable name contains multiple words, use dashes to separate them. Names can contain any character, including punctuation, with the exception of quotes and grouping symbols like parentheses. Even `:`, `+` and `->` are variables!
