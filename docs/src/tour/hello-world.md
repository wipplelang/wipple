# Hello, world!

To display text on the screen, use `show`:

```wipple
show "Hello, world!"
```

```wipple-output
Hello, world!
```

Text is written inside double quotes (`"..."`).

`show` accepts numbers, too:

```wipple
show (1 + 2)
```

```wipple-output
3
```

You need parentheses there because operators like `+` have precedence over whitespace, so `show 1 + 2` is interpreted as `(show 1) + 2`. Rather than displaying 1 on the screen and adding the result to 2, we want to add 1 to 2 and display the result. You can use parentheses like this almost anywhere!
