# For JavaScript Developers

Welcome to Wipple! This guide goes over some basic JavaScript concepts and their equivalent in Wipple. When you finish this guide, you'll have a foundational understanding of Wipple code that you can use to experiment on your own.

## Hello, world

Wipple's equivalent of `console.log` is `show`:

```wipple
show "Hello, world!"
```

Notice that there's no semicolons in Wipple code — just put each statement on its own line.

## Comments, numbers and strings

You can write a comment using `--`. Wipple only has line comments:

```wipple
-- This is a comment
this is executed -- this is not
```

Numbers are represented in base 10 instead of floating point, but they are written the same way:

```wipple
42
3.14
-1
```

Strings are called "text" in Wipple, and must use double quotes:

```wipple
"Hello, world!"
"line 1\nline 2"
```

You can use `format` to do string interpolation:

```wipple
format "Hello, _!" "world" -- Hello, world!
```

## Variables

In Wipple, you can declare variables using the `:` operator:

```wipple
answer : 42
name : "Wipple"
```

Wipple uses static single assignment, which means that you can't change the value of an existing variable after you create it. However, you can declare the same variable twice — the new variable shadows the old one:

```wipple
x : 42
x : x + 1
show x -- 43
```

## `if` statement

Wipple doesn't have an `if` statement like in JavaScript. Instead, `if` works more like the ternary operator, and can be used anywhere an expression is needed. By convention, boolean variables end in a question mark.

```wipple
password : "letmein123"
valid? : password = "password123!" -- use a single '=' to compare values
show (if valid? "Access granted" "Access denied") -- Access denied
```

## Basic types

Wipple is a strongly-typed language, which means that your code is verified at compile-time. Luckily, Wipple has type inference, so you usually don't need to think about types at all! You can use `::` to annotate the type of a value.

```wipple
42 :: Number
"Hello" :: Text
```

If you mismatch the types, Wipple will emit an error:

```wipple
42 :: Text -- Mismatched types: Expected Text, found Number
```

## Objects

Wipple calls objects "data structures", which you can create using `data`:

```wipple
Person : data {
    name :: Text
    age :: Number
}
```

You can create an instance of this object like so:

```wipple
bob : Person {
    name : "Bob"
    age : 35
}
```

## Functions

Wipple's functions work like JavaScript's arrow functions. In fact, they both use the arrow notation!

```wipple
increment : x -> x + 1
show (increment 42) -- 43
```

One big difference is that Wipple functions may only accept a single parameter. If you want multiple parameters, use multiple functions!

```wipple
add : a -> b -> a + b
show (add 1 2) -- 3
```

If that's confusing, here's the equivalent JavaScript code:

```javascript
const add = (a) => (b) => a + b;
console.log(add(1)(2)); // 3
```

## Methods

Wipple doesn't allow you to add methods to an object (although you can store functions inside data structures like any other value). Instead, you can declare functions like this:

```wipple
greet : person :: Person -> format ("Hello, _!") (person name)
greet bob -- Hello, Bob!
```

Alternatively, you can use the `.` operator to chain function calls:

```wipple
bob . greet -- Hello, Bob!
```

## Inheritance

Wipple has neither classes nor inheritance. Instead, you can use traits! Traits are pretty advanced, but here's a simple example:

```wipple
-- Greet is a trait that can be defined with a function returning text
Greet : for A -> trait (A -> Text)

-- For any value on which Greet is defined, return a greeting
greet : for A where (Greet A) -> x :: A -> format "Hello, _!" (Greet x)


Person : data {
    name :: Text
}

-- Greet for Person values is defined as the person's name
Greet Person : p -> p name


Earth : data

-- Greet for Earth values is defined as "world"
Greet Earth : _ -> "world"


greet (Person { name : "Bob" }) -- Hello, Bob!
greet Earth -- Hello, world!
```
