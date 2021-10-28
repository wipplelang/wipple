# Introduction

Welcome to Wipple! Wipple is a programming language for building DSLs that's natural to read, write and learn.

You can use this documentation to learn how to write Wipple code, how to express concepts from other programming languages in Wipple, and how to manage your own Wipple projects.

# Philosophy

I ([Wilson Gramer](https://gramer.dev)) initially started building Wipple to learn how programming languages work. Every language is built to satisfy their creator's vision of a "perfect" way of expressing code — and Wipple is no exception! — but I noticed that often this perfection is tarnished by reasons of practicality or history. Therefore, Wipple's design philosophy is **no magic**: builtin constructs are just regular functions and have no special syntax, making it easy to learn through experimentation.

I've designed Wipple under the same assumption as [Bel](http://www.paulgraham.com/bel.html) — "if computers were as powerful as we wanted, what would programming languages look like?" Wipple code is about **describing your ideas** more than it is about how those ideas are actually executed or represented in memory by the computer. Even constructs like booleans are represented using variants (sum types) in Wipple, because truth and falsehood are axioms — even if computers store them as a 1 or 0, logically they are defined in terms of their relationships to other values.

Wipple also takes a more practical approach than purely functional languages like Haskell or Idris — even if describing side effects in terms of some kind of `IO` construct is more "correct", I think it's a lot harder to teach. Mutation is allowed, for example, but Wipple will guide you toward using functional concepts most of the time instead. And even if representing all code using only lists and atoms is more "pure" (or even, arguably, more beautiful), I think having statements and operators make code lot easier to read and write. Finally, to the end user, Wipple has sensible defaults and little configuration: all Wipple code is formatted the same, the standard library is tiny, versioning is handled in your own repository instead of through a package manager, and you can start writing code in the [Wipple Playground](https://playground.wipple.gramer.dev) right now from any device. To the hacker, though, you can shape Wipple to do almost anything you want.

Wipple is written in Rust, and the project is [available on GitHub](https://github.com/wipplelang/wipple). Contributions are welcome! Because Wipple is primarily an experiment for me, there are no current plans for source or API stability. However, just because Wipple focuses on expression of ideas doesn't mean it can't also be fast and bug-free!

With that out of the way, let's write some Wipple code!
