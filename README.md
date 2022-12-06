<p align="center">
  <img src="website/home/images/logo.svg">
</p>

<h1 align="center">
  The Wipple Programming Language
</h1>

<p align="center">
  Wipple is a programming language that’s natural to read, write and learn.
</p>

## Learn Wipple

-   Visit the [Wipple Playground](https://playground.wipple.gramer.dev) and click Learn in the top right for Wipple tutorials.
-   Visit the [Wipple Guide](https://guide.wipple.gramer.dev) for quick start guides, tutorials for advanced concepts, and the language reference.

## Development workflow

The Wipple project is split across several folders. The compiler and tooling are written in [Rust](https://rust-lang.org), and the playground is written in [Next.js](https://nextjs.org). The guide is published using [mdBook](https://github.com/rust-lang/mdBook).

You can use these commands to build or test Wipple:

```shell
# Test using a local file
cargo run --bin wipple -- run path/to/test.wpl

# Test with diagnostic tracing enabled
cargo run --bin wipple -- run path/to/test.wpl --trace

# Run automated tests
cargo run --bin wipple-test -- tools/test/tests

# Run the playground on a local development server
cd website/playground && npm install && npm run dev

# View the documentation on a local development server
cd website/guide && mdbook serve
```
