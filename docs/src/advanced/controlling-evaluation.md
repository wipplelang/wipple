# Controlling evaluation

Wipple is a **homoiconic** programming language, which means that "code is data". Lists, for example, serve both as data structures (eg. `("Alice" "Bob" "Charlie")`) and as function calls (eg. `(add 2 3)`). So how does this actually work?

Some Wipple values have the `Evaluate` trait, which is used by the interpreter to take a value and produce another value in its place. Lists (ie. `List` values) derive `Evaluate` and implement it as a function call. This makes Wipple code very powerful — you can create new code at runtime and run it!

For example, the `debug` function is defined as the following:

```wipple
debug : x => show (format "_ ==> _" (literal x) x)
```

This code looks familiar for the most part, except two differences — instead of using `->` we use `=>`, and there's the use of the `literal` function. Let's look at what happens when you write `debug (1 + 2)`!

As you may have seen a few times already, you can use a single quote (`'`) before a value to denote "literally this value". For example, `'a` is evaluated as the name `a` and not the value of the variable `a`. In technical terms, the evaluation of `a` is delayed.

This is basically what `=>` does: when you pass in a value as input, the value is not evaluated. Then the interpreter does find and replace — if the parameter is named `foo`, then all occurrences of `foo` in the return value are replaced with the input. Finally, the return value is evaluated in place of the function call. `=>` creates a function that behaves in this way, known as a **template**.

You might ask why we use `literal x` instead of `'x` in the implementation of `debug` — remember that literal values really do mean "literally", so it wouldn't make sense to replace them with a template input. Take a look at what happens in this code:

```wipple
-- Create a list containing two copies of the input
double : x => '(x x)

double 2 -- (x x)
```

Instead of creating the list as desired, `double` just returns `(x x)`. To work around this problem, you can use the `literal` function, which returns its input unevaluated (just like quoting):

```wipple
-- Create a list containing two copies of the input
double : x => literal (x x)

double 2 -- (2 2)
```

The difference is that quoting is a syntactical operation, while `literal` is just a regular function call that happens at runtime. This means that the find-and-replace used by templates has no knowledge of `literal`, and can't prevent its input from being replaced.

Keep in mind that the return value of templates is still evaluated! So this code won't work:

```wipple
-- Create a list containing two copies of the input
double : x => (x x)

double 2 -- error: cannot call '2' because it is not a function
```

Templates are useful when you want to operate on "code" instead of "data"; that is, you want to control how the input is evaluated. That's why `if` is a template instead of a closure:

```wipple
if : condition then else => match (condition as Condition) {
  True : then
  False : else
}
```

If `if` evaluated its inputs as they were passed into the function, like in closures, then both `then` and `else` would be evaluated! This can cause trouble when the input to `if` has side effects:

```wipple
bad-if : Condition condition -> then -> else -> match condition {
  True : then
  False : else
}

-- displays both "Woohoo!" and "Oh no!"
bad-if True (show "Woohoo!") (show "Oh no!")
```

Of course, in this example you could just do `show (bad-if True "Woohoo!" "Oh no!")`, but if you want `bad-if` to accept _any arbitrary_ code, it's more correct to delay evaluation by using a template.

In general, you should use functional programming concepts in your code, and evaluation will "just work". But templates are useful in a few cases:

- When you want to delay evaluation of your input

- When you want to do something directly in the caller's scope, like declaring a variable

  - For example:
    ```wipple
    increment! : x => (x : x + 1)
    ```

- When you're implementing a DSL that evaluates code in custom ways

## How templates work: interpolation

The find-and-replace operation done by templates may seem like magic. Of course, Wipple's philosophy is **no magic** — here's how it actually works!

First, let's take a look at the `format` function:

```wipple
greet : format "Hello, _!"
```

The `format` function takes a text value as input, and then returns a function you can use to replace the `_`s in the text. So we can call `greet` like this:

```wipple
greet "Bob" -- Hello, Bob!
```

This process of splicing the text, replacing certain portions of it, and then combining it back together is called **interpolation**, and you may be familiar with it in other languages (eg. Python's f-strings or Rust's `format!` macro). Wipple uses a similar process for arbitrary values as well!

Values can derive the `Interpolate` trait, which is responsible for handling interpolation. There are two "modes" of interpolation, literal and direct:

- **Literal interpolation** happens inside a literal. For example, the escaped value `\b` in `'(a \b c)` is interpolated by evaluating `b`. Lists are interpolated by interpolating each item in the list, so you can use escaped values recursively: `'(a (b \c (\d e)) f)`.
- **Direct interpolation** happens when an escaped value is evaluated directly, eg. `\(a b c)`. In this case, values that can be interpolated directly are interpolated, and values that can't are evaluated. This creates a distinction between "structural" and "terminal" values — lists, for example, operate on their contents, while names are evaluated in of themselves. Direct interpolation is also recursive: `\(a (c d (d e)) f)`.

Templates work by directly interpolating their return value, and then evaluating the interpolated result in the calling environment.

## Interpolation semantics by example

If you're confused, this table might help: (But first [open an issue]() to help improve this documentation! 😄)

| INPUT              | OUTPUT<br /><small>Assume `a : 1`, `b : 2` and `c : 3`.</small> |
| ------------------ | ------------------------------------------------------------ |
| `a`                | `1` — regular evaluation                  |
| `'a`               | `a` — literals                                               |
| `''a`              | `'a`                                       |
| `'(\a b c)`        | `(1 b c)` — literal interpolation |
| `'(\a \b \c)`      | `(1 2 3)`                                                   |
| `'\a`              | `1` — escaping undoes quoting |
| `'((\a b c) b c)`  | `((1 b c) b c)`                                           |
| `''\a`             | `'\a` — literals are not interpolated within literals |
| `'('(\a b c) b c)` | `('(\a b c) b c)`                                         |
| `\(a b c)`       | `(1 2 3)` — direct interpolation |
| `\'a`            | `a` — literals are not directly interpolated |
| `\('a b c)`   |`(a 2 3)`|
| `\\a`            | `1` — escaping a value twice has no effect |
| `\(\a b c)`      | `(1 2 3)`    |
| `\('(\a b c) b c)` | `((1 b c) 2 3)` — literals are interpolated directly |

## Accepting multiple inputs in a template

Templates inputs are declared differently than in closures — instead of using multiple closures to accept multiple inputs and currying them, templates define all inputs at once:

```wipple
double-3 : a b c => literal (a a b b c c)
double-3 1 2 3 -- (1 1 2 2 3 3)
```

You can still partially apply templates, but the evaluation of the return value will only be performed when the last input is given. This can lead to surprising semantics if you aren't careful:

```wipple
first : a b => a

partial : {
  x : 1
  first x
}

result : {
  x : 2
  partial _
}

result -- 2, not 1
```

Even though `x` was provided to the template in `partial`'s scope, the return value of `first` is only evaluated after all the inputs are given (which happens in `result`'s scope). To avoid this, it's generally a good idea to keep templates limited to a single input (eg. using a list), or to provide all inputs at once (eg. use `if` for two branches and `when` for one). If you need stronger semantics, you should probably refactor your program to use closures that operate on evaluated inputs.

