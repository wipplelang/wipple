<p align="center">
  <img src="website/home/images/logo.svg">
</p>

<h1 align="center">
  The Wipple Programming Language
</h1>

<p align="center">
  Wipple is a programming language that’s natural to read, write and learn.
</p>

<p align="center">
  <a href="https://github.com/wipplelang/wipple/actions/workflows/tests.yml">
    <img src="https://github.com/wipplelang/wipple/actions/workflows/tests.yml/badge.svg" alt="tests">
  </a>

  <a href="https://github.com/wipplelang/wipplelang.github.io/actions/workflows/deploy.yml">
    <img src="https://github.com/wipplelang/wipplelang.github.io/actions/workflows/deploy.yml/badge.svg" alt="website">
  </a>
</p>

## Overview

### Natural

Wipple was designed with just a few rules and minimal punctuation, making it a great language for students.

```wipple
greet : name -> "Hello, _!" name
show (greet "world") -- Hello, world!
```

### Expressive

Wipple’s functional style and extensible constructs enable you to describe and manage complex data with ease.

```wipple
report-card :: Grade -> Text
report-card : grade -> when grade {
  A -> "top of the class"
  B -> "good job"
  C -> "need to study"
  D or F -> "didn't pass"
}
```

### Powerful

Wipple’s type system automatically checks your mistakes to prevent bugs. And with traits, you can add new functionality to others’ code.

```wipple
Person : type {
  name :: Text
  age :: Number
}

instance (Show Person) : { name age } -> \
  "_ is _ years old" name age

bob : Person {
  name : "Bob"
  age : 30
}

show bob -- Bob is 30 years old
```

## Learn Wipple

The [Wipple Playground](https://wipple.dev/playground) has over 30 lessons for programmers of all experience levels. Try it out right in your browser!

## Build Wipple

The Wipple project is split across several folders. The compiler and CLI are written in [Rust](https://rust-lang.org), and the playground is written in [React](https://react.dev). The compiler documentation is published using [mdBook](https://github.com/rust-lang/mdBook).

You can use these commands to build and test Wipple locally. Make sure you have [`task`](https://taskfile.dev) installed.

```shell
# Compile and run a local file
task run -- path/to/file.wpl

# Compile and run a local file with diagnostic tracing enabled
task run -- path/to/file.wpl --trace

# Compile a local file and inspect the optimized IR
task compile -- path/to/file.wpl -O -f ir

# Run automated tests
task test

# Install 'wipple' in your PATH
task cli

# Serve the website on http://localhost:8080
task serve
```
