# Intrinsics

Wipple uses intrinsics to give the compiler special knowledge of certain types, traits and constants. For example, whenever the `+` operator is used, Wipple calls the function defined by the `add` intrinsic — in the standard library, this is the `Add` trait.

To define an intrinsic, use the `intrinsic` keyword, followed by the type of value (`type`, `trait`, or `constant`) and its name:

```wipple
intrinsic "trait" "add" : Add
```

The compiler assumes that the associated type, trait, or constant has the correct shape and behavior; if it doesn't, the compiler may produce invalid code, strange type errors, or crash.

Here's a list of the intrinsics used in Wipple:

| Name                    | Type     | Value                   | Used By                 |
| ----------------------- | -------- | ----------------------- | ----------------------- |
| `add`                   | trait    | `Add`                   | The `+` operator        |
| `and`                   | trait    | `And`                   | The `and` operator      |
| `as`                    | trait    | `As`                    | The `as` operator       |
| `boolean`               | type     | `Boolean`               | The runtime             |
| `build-collection`      | constant | `Build-Collection`      | The `,` operator        |
| `by`                    | trait    | `By`                    | The `by` operator       |
| `divide`                | trait    | `Divide`                | The `/` operator        |
| `equal`                 | trait    | `Equal`                 | The `=` operator        |
| `error`                 | trait    | `Error`                 | Custom error messages   |
| `false`                 | constant | `False`                 | The runtime             |
| `greater-than-or-equal` | trait    | `Greater-Than-Or-Equal` | The `>=` operator       |
| `greater-than`          | trait    | `Greater-Than`          | The `>` operator        |
| `hasher`                | type     | `Hasher`                | The runtime             |
| `initial-collection`    | constant | `initial-collection`    | The `,` operator        |
| `is-equal-to`           | constant | `Is-Equal-To`           | The runtime             |
| `is-greater-than`       | constant | `Is-Greater-Than`       | The runtime             |
| `is-less-than`          | constant | `Is-Less-Than`          | The runtime             |
| `less-than-or-equal`    | trait    | `Less-Than-Or-Equal`    | The `<=` operator       |
| `less-than`             | trait    | `Less-Than`             | The `<` operator        |
| `list`                  | type     | `List`                  | The runtime             |
| `maybe`                 | type     | `Maybe`                 | The runtime             |
| `multiply`              | trait    | `Multiply`              | The `*` operator        |
| `none`                  | constant | `None`                  | The runtime             |
| `not-equal`             | trait    | `Not-Equal`             | The `/=` operator       |
| `number`                | type     | `Number`                | Type of number literals |
| `or`                    | trait    | `Or`                    | The `or` operator       |
| `ordering`              | type     | `Ordering`              | The runtime             |
| `power`                 | trait    | `Power`                 | The `^` operator        |
| `remainder`             | trait    | `Remainder`             | The `%` operator        |
| `show`                  | constant | `Describe`              | `_` in text literals    |
| `some`                  | constant | `Some`                  | The runtime             |
| `subtract`              | trait    | `Subtract`              | The `-` operator        |
| `task-group`            | type     | `Task-Group`            | The runtime             |
| `text`                  | type     | `Text`                  | Type of text literals   |
| `to`                    | trait    | `To`                    | The `to` operator       |
| `true`                  | constant | `True`                  | The runtime             |

The runtime also exposes many functions that are called with `intrinsic`; see [the runtime source code](https://github.com/wipplelang/wipple/blob/main/interpreter/src/intrinsics.ts).
