# Hello, world!

To display text on the screen, use `show`:

```wipple
show "Hello, world!"
```

Text is written inside double quotes (`"..."`).

Throughout the tour, comments (`--`) are used to indicate the output of a line of code. So we can write the above like this:

```wipple
show "Hello, world!" -- Hello, world!
```

`show` accepts numbers, too:

```wipple
show (1 + 2) -- 3
```

You need parentheses there because operators like `+` have precedence over whitespace, so `show 1 + 2` is interpreted as `(show 1) + 2`. Rather than displaying 1 on the screen and adding the result to 2, we want to add 1 to 2 and display the result. You can use parentheses like this almost anywhere!
