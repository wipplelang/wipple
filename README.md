<p align="center">
  <img src="website/home/images/logo.svg">
</p>

<h1 align="center">
  The Wipple Programming Language
</h1>

<p align="center">
  Wipple is a programming language that’s natural to read, write and learn.
</p>

## Overview

### Natural syntax

Wipple’s syntax was designed with just a few rules and balances punctuation with English words, making it a great first programming language. Instead of sticking with conventional names like `print`, Wipple uses more intuitive names like `show`.

```wipple
greet : name -> format "Hello, _!" name
show (greet "world") -- Hello, world!
```

### Type system

Wipple has a powerful type system that checks your mistakes to prevent crashes at runtime. You can document a function’s behavior using a type annotation, or let Wipple figure out the types for you. And you can use traits to extend existing code with new behavior.

```wipple
Person : type {
    name :: Text
    age :: Number
}

instance (Show Person) : { name age } ->
    format "_ is _ years old" name age

bob : Person {
    name : "Bob"
    age : 30
}

show bob -- Bob is 30 years old
```

### Pattern matching

Instead of using `if`, Wipple’s `when` expression enables you to check complex conditions with ease. Before your code even runs, Wipple ensures that it handles all possible conditions — no surprises in production.

```wipple
report-card :: Grade -> Text
report-card : grade -> when grade {
    A -> "top of the class"
    B -> "good job"
    C -> "need to study"
    D or F -> "didn't pass"
}
```

## Learn Wipple

The [Wipple Playground](https://wipple.dev/playground) has over 30 lessons for programmers of all experience levels. Try it out right in your browser!

## Build Wipple

The Wipple project is split across several folders. The compiler and CLI are written in [Rust](https://rust-lang.org), and the playground is written in [React](https://react.dev). The compiler documentation is published using [mdBook](https://github.com/rust-lang/mdBook).

You can use these commands to build and test Wipple locally:

```shell
# Test using a local file
cargo run --bin wipple -- run path/to/test.wpl

# Test with diagnostic tracing enabled
cargo run --bin wipple -- run path/to/test.wpl --trace

# Run automated tests
cargo run --bin wipple-test -- tests

# Serve the website on a local development server (requires https://taskfile.dev)
task dev
```
