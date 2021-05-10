# Traits

If you've used other languages like Java, C or TypeScript, you may have noticed a glaring omission in Wipple code — types! Wipple is a dynamically-typed language, but is still very strict about what values can be used where. This is accomplished using **traits**, which describe the representations and behavior of values.

All Wipple values have a trait — here are some of the common ones:

| **Value**       | **Trait**  |
| --------------- | ---------- |
| `foo`           | `Name`     |
| `42`            | `Number`   |
| `"hello"`       | `Text`     |
| `'foo`          | `Literal`  |
| `(a b c)`       | `List`     |
| `{ a b c }`     | `Block`    |
| `a -> b `       | `Function` |
| `new { a : b }` | `Module`   |
| `_`             | `Empty`    |
| `Name`          | `Trait`    |

You can create your own traits using the `trait` function. By convention, traits are capitalized:

```wipple
Person : trait _
```

For now, ignore the `_` — we will get to that in the next section.

Traits are also functions, and can be called with a value to create a value of the trait:

```wipple
bob : Person "bob"
```

Right now, traits don't seem very useful. Let's move on to the next section, where we'll learn how to use traits as patterns!
