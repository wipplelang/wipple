# Changelog

## Unreleased

-   Wipple now compiles to JavaScript instead of an interpreted IR.
-   Added back the `wipple` CLI, using `node` as the default runtime.
-   Added the `go` command to the Turtle runtime, and show the mouse's coordinates when hovering over the canvas.

## Wipple 2025.1 (2025-07-24)

-   In the Wipple Playground, the compiler now runs on Netlify Functions and the frontend has been rewritten in Svelte. Error messages now appear inline, drag-and-drop is more intuitive, and the code editor is significantly faster.

-   The compiler is now a single Rust crate, is no longer generic over a `Driver`, and now builds incrementally.

-   The `wipple` CLI and VSCode extension have been removed and will return soon.

## Wipple 1.0 (2024-06-21)

-   New compiler with a C-like build system

-   Redesign playground with Google Docs-like interface, significantly improved syntax highlighting, and support for dropdowns and other inline widgets

-   Several new lessons in the playground, including a physics module

## Wipple 0.12 (2023-12-30)

### Language

-   String interpolation no longer requires the `format` keyword

-   Improved diagnostic for building list with mismatched elements

-   Implicit imports remove the need to `use` dependencies in the playground

-   Custom entrypoints with the `[entrypoint]` attribute

-   `[help-show-code]` attribute displays the source code of expressions with the wrong type (eg. `` cannot display `basketball` on the screen `` instead of `` cannot display `Sport` on the screen ``)

-   Add fix-it to insert parentheses around mistyped expressions containing operators

-   Typechecker now tracks the reason why a type was inferred

-   Allow defining code snippets with the `snippet` keyword

-   Numbers can now be `undefined` instead of crashing the program

-   Tuples are now written using `;` (`,` is now for collections and uses the `Build-Collection` trait)

-   Types can now be `[sealed]`, preventing their construction outside the file in which they are defined

-   Added `|` operator for composing functions

-   Added `Hash`, `Dictionary` and `Set`

-   Typechecker is more reliable and requires fewer type annotations

-   Improved code formatter that collapses lists to a single line

-   Parser now accepts multi-line statements when there's an operator at the beginning or end of a line

-   Numbers now support units (eg. `1 seconds` is equivalent to `seconds 1`, where `seconds :: Number -> Duration`)

### Playground

-   New course-oriented design and layout

-   New Energy Use and Physics modules

-   New standalone app powered by Electron

-   Improved display of error messages

-   Editor now detects emoji assets and shows an emoji picker when clicked

-   Improved live collaboration

-   Added support for auto-completion

-   Insert button now appears inline

-   Added focus mode that dims inactive lines in the code editor

-   When a program contains errors, output is now persisted until the program is valid again

## Wipple 0.11 (2023-09-03)

### Language

-   Updated looping API using `repeat` syntax

-   Load all of a file's dependencies in parallel

-   Raise error when extra expressions are provided to a `syntax` rule

-   Improved error reporting inside expanded `syntax` rules

-   `syntax` rules are now hygienic

-   Add `[private]` attribute

-   Improved type inference involving default types for type parameters

-   Allow acronyms in naming conventions lint

-   Allow merging structures using `where`

-   Detect redundant `when` cases

-   Allow partial application of operators

-   Adjust syntax for placeholders so they don't conflict with the multiplication operator

-   Add `[sealed]` instances

-   Support block repetition syntax patterns with `...{}`

### Standard Library

-   Rename `insert` and `remove` to `insert-at` and `remove-at`

-   Add `toggle!`

-   Add `[convert-from]` to `Maybe` and `Result`

-   Implement `Random` for `Boolean`

-   Add `negate`

-   Rename `Ok` to `OK`

-   Add more default types for type parameters, so manual type annotations are required less often

-   Add more specialized implementations of standard library functions like `join` and `count`

-   Add `not` operator for functions

-   Add `expect` for booleans

-   Rename `crash` to `error`

-   Support partial application for most standard library operators

-   Rename `iterator` to `sequence` (and related APIs)

-   Add `sample` and `take` sequence functions

-   Add `find` and `find-index` sequence functions

-   Add `all?` and `any?` sequence functions

-   Add `divisible-by?` to check if a number is divisible by another number

-   Lists with mismatched element types now show the error on each mismatched element rather than on the `list` syntax rule

### Playground

-   Implement collaborative editing

-   Add game engine inspired by SmileBASIC under the `game` library

-   Auto-close brackets and auto-indent when beginner mode is off

-   Add lessons for every warning and error message

-   Update beginner lessons and add graphic to top of lessons

-   Add note asset literals and allow choosing a new note using a keyboard

### Tooling

-   Updated test runner — tests are now specified in their own `.wpl` files, and the output is specified in `.stdout` and `.stderr` files

-   LSP now supports go-to-definition, go-to-references, and renaming of local symbols

## Wipple 0.10 (2023-07-29)

### Language

-   Allow splitting statements across multiple lines using `\`

-   Allow defining simple enums on a single line

-   Added asset literals for inline colors and images

-   Added `end` for returning from a function early

-   Added type aliases

-   WIP support for compiler plugins

### Standard Library

-   More iterator functions

-   Added randomness functions

-   Support `by` on ranges to form a stride (eg. `1 to 10 by 2`)

### Playground

-   New playground code editor based on CodeMirror with support for asset literals

### Tooling

-   New documentation generator and published documentation for language primitives, `std`, `turtle`, `music` and `graphing`

-   WIP documentation on the Wipple compiler

## Wipple 0.9 (2023-06-16)

-   Simplified syntax: removed tab-based indentation and the need for spaces between names and operators

-   Added code formatter

-   Implement inferred type parameters that improve type inference when using traits like `Add` and `Iterate`

-   Added the `[convert-from]` attribute to specify a fix-it for converting from one type to another

-   Updated playground code editor layout with support for formatting and searching the list of available declarations

-   Expanded the types of declarations that support the `[help]` attribute

-   Added placeholder expressions that must be filled before running the program

## Wipple 0.8 (2023-04-28)

-   Added support for interactivity, including `prompt`, `choice`, and custom JavaScript-based UI elements in the playground

-   Diagnostics for beginner mistakes like forgetting to quote a `Text` value or using `print` instead of `show`

-   Added turtle graphics, music and graphing to the playground

-   Functions that accept multiple inputs may now list all inputs to the left of a single arrow. This is just syntax sugar — all functions may still be partially applied

-   Enumeration types now have their variants exposed by default, removing the need to `use` them

-   Implemented default type parameters, useful for functions like `prompt` and `crash`

-   Rewritten exhaustiveness checking algorithm properly handles all cases

-   New functions for working with iterators, including `count`, `split`, `iterator` and `next`

-   Removed `if?` in favor of `if` defaulting the else branch to `()`

-   Allow specifying a custom message when a trait isn't implemented using `[on-unimplemented]`

-   Many improvements to the playground, including beginner mode and 30 new interactive lessons

-   Many bug fixes related to type checking and optimization (including tail call optimization)

## Wipple 0.7 (2023-02-15)

-   Rewritten AST expander with context-dependent grammar and (work-in-progress) syntax hygiene

-   `syntax` definitions replace templates and offer a more flexible way to define new syntax and operators

-   `list` syntax now accepts values directly instead of requiring that they be wrapped in a tuple

-   Statements in blocks now automatically default to a type of `()` if a different type cannot be deduced

-   The compiler recursion limit can be set with a new `[[recursion-limit]]` attribute

-   The Wipple Playground now supports semantic highlighting and documentation tooltips

## Wipple 0.6 (2023-01-18)

-   Improved syntax for dealing with types, traits and instances, including implicit type parameters and more consistent `instance` and `where` syntax

-   Added support for type-level programming with type-level traits, overlapping instances, and improved type inference for bounded type variables

-   Added several attributes for types and traits that allow you to control the error messages produced by the type checker

-   Specialized constants allow you to override the implementation of a generic constant for a more specific type to improve its performance

-   Improved exhaustiveness checking now tells you which cases are missing from an incomplete `when` expression

-   `end` expression allows you to exit from a function early

-   `of` operator allows you to retrieve a single field from a structure value without destructuring

-   Implemented several IR optimizations, including inlining of functions

-   Added an option in the Wipple Playground to lock a section of text to prevent accidental editing, as well as an option to disable linting for a particular section of code

-   Updated the Wipple Guide with tutorials for people coming from other languages, as well as an Advanced Wipple section and an updated reference

-   Fixed many bugs, crashes and infinite loops throughout the compiler

## Wipple 0.5 (2022-10-19)

-   Support recursive instances and constants

-   Implement various numeric types, including `Integer`, `Natural` and `Float`/`Double`

-   IR is now stack-based and supports tail call optimization, allowing `loop` and others to be expressed as regular recursive functions

-   Refactor typechecker to be more error tolerant

-   Add language server to replace VSCode extension

## Wipple 0.4 (2022-08-30)

-   Implement FFI for calling C functions from Wipple using `external "c"`

-   Allow importing specific names from a file using `{ a b c } : use "foo.wpl"`

-   Types, traits and constants may now be specified in any order

-   Types may now refer to themselves recursively

## Wipple 0.3.1 (2022-07-27)

-   Fixed build script

## Wipple 0.3 (2022-07-27)

-   Wipple now compiles to an intermediate representation (IR) that can be further compiled to a variety of backends

-   Added Rust backend that compiles Wipple IR to an optimized native binary using LLVM

-   `wipple run` now uses the Rust backend by default

-   Added `wipple dump` command to print analysis information, IR, or generated Rust code

-   Added VSCode extension with semantic highlighting, hovering and autocompletion

-   Added `[keyword]` attribute that instructs IDEs to treat the provided template as a keyword

-   Fixed some bugs and crashes in the typechecker

## Wipple 0.2 (2022-07-03)

-   Statements are now treated as expressions when they are surrounded in parentheses

-   Improved diagnostics for missing instances, including the new `[on-unimplemented]` attribute

-   `when` and patterns are now checked for exhaustiveness

-   Standard library is no longer bundled and is instead loaded at compile-time from https://pkg.wipple.gramer.dev/std/std.wpl

-   Instances are now checked to ensure they don't collide with other instances

-   Tuples replace the quoted list syntax; you can create a list from a tuple using the `list` function

## Wipple 0.1 (2022-06-25)

-   Implement compiler with static typechecking and support for templates, operators and traits

-   Implement the [Wipple Playground](https://playground.wipple.gramer.dev) with a notebook-style interface and included tutorials

-   Add the [Wipple Guide](https://guide.wipple.gramer.dev) with quick start guides, tutorials for advanced concepts, and the language reference
