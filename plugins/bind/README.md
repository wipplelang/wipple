# Create Wipple bindings to Rust

This library lets you easily create Wipple bindings for Rust functions.

Currently the following types are implemented:

| Rust type        | Wipple trait                               |
| ---------------- | ------------------------------------------ |
| `i*`, `u*`, `f*` | `Number` (bounds check is done at runtime) |
| `()`             | `Empty`                                    |
| `String`         | `Text`                                     |

To create a binding, use the `bind!` macro:

```rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}

env.set_variable("add", bind!(fn add(i32, i32) -> i32));
```

You can use `define_bindings!` to quickly define multiple bindings:

```rust
define_bindings!(env, {
    fn add(i32, i32) -> i32;
    "do-something!" => fn do_something();
});
```

## Examples

| Rust input                                    | Generated Wipple code (`<...>` indicates call to Rust code)                                                                |
| --------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- |
| `bind!(fn foo())`<br>`bind!(fn foo() -> i32)` | Rust closure that can be called with an `env` and `stack` — normally you pass this to `Environment::set_computed_variable` |
| `bind!(fn foo(i32))`                          | `Number a -> <foo(a)>`                                                                                                     |
| `bind!(fn foo(i32) -> i32)`                   | `Number a -> Number for <foo(a)>`                                                                                          |
| `bind!(fn foo(i32, i32))`                     | `Number a -> Number b -> <foo(a, b)>`                                                                                      |

## Note on strings and other borrowed values

Bound Rust functions must consume their inputs, so you should make your function accept a `String` instead of a `&str`.

## Note on unsafe functions

Unsafe functions are allowed to decrease friction when working with C bindings. (Omit `unsafe` when binding to the function.) Users of the binding must still ensure that calling the function is sound — if you need to do any preparation before/after calling unsafe functions (eg. freeing memory), you should make a safe wrapper function and bind to that instead, or push the safety requirements into the Wipple code (eg. binding to a `free` function and mentioning that it needs to be called in your documentation).

## Conventions

Any function with side effects should be named with an exclamation mark (eg. `my-function!`).

## Adding conversions for your own types

You can implement the `FromValue` and `AsValue` traits to convert between Wipple values and Rust types. For example:

```rust
#[derive(Debug, Clone)]
pub struct Foo { ... };

primitive!(pub foo for Foo);

impl FromValue for Foo {
    fn from_value(value: Value, env: &Environment, stack: &Stack) -> Result<Self> {
        value.get_or::<Foo>("Expected Foo value", env, stack)
    }
}

impl AsValue for Foo {
    fn as_value(&self, _: &Environment, _: &Stack) -> Result {
        Ok(Value::of(self.clone()))
    }
}

fn bar(foo: Foo) -> Foo {
   ...
}

env.set_variable("bar", bind!(fn bar(Foo) -> Foo));
```
