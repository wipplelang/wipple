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
if : condition => then => else => match (condition as Condition) {
  True : then
  False : else
}
```

If `if` evaluated its inputs as they were passed into the function, like in closures, then both `then` and `else` would be evaluated! This can cause trouble when the input to `if` has side effects:

```wipple
bad-if : condition Condition -> then -> else -> match condition {
  True : then
  False : else
}

-- displays both "Woohoo!" and "Oh no!"
bad-if True (show "Woohoo!") (show "Oh no!")
```

Of course, in this example you could just do `show (bad-if True "Woohoo!" "Oh no!")`, but if you want `bad-if` to accept *any arbitrary* code, it's more correct to delay evaluation by using a template.

In general, you should use functional programming concepts in your code, and evaluation will "just work". But templates are useful in a few cases:

- When you want to delay evaluation of your input

- When you want to do something directly in the caller's scope, like declaring a variable
  - For example:
    ```wipple
    increment! : x => (x : x + 1)
    ```
  
- When you're implementing a DSL that evaluates code in custom ways

> **What about quasiquoting?** If you're coming from Lisp you may miss this! In an effort to reduce complexity and keep the syntax small, Wipple doesn't have builtin support for quasiquoting. If templates don't serve your needs, you can implement it yourself in code or by using an interpreter plugin!

