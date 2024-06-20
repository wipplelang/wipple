<p align="center">
  <img src="web/home/images/logo.svg">
</p>

<h1 align="center">
  The Wipple Programming Language
</h1>

<p align="center">
  <a href="https://github.com/wipplelang/wipple/actions/workflows/test.yml">
    <img src="https://github.com/wipplelang/wipple/actions/workflows/test.yml/badge.svg?branch=main" alt=".github/workflows/test.yml">
  </a>
  <a href="https://codecov.io/gh/wipplelang/wipple" >
    <img src="https://codecov.io/gh/wipplelang/wipple/branch/main/graph/badge.svg?token=45CTMYY78V"/>
  </a>
<p>

Wipple is an educational programming language designed by [Wilson Gramer](https://gramer.dev). With Wipple, you can make art and music, explore math and science, design video games, and more.

You write Wipple code in the [Wipple Playground](https://wipple.dev/playground), an interactive, collaborative, online coding platform. The playground works on any device with no setup required. With 80+ lessons and guides, there’s something for everyone.

Wipple code is natural to read and write, and the language is designed to grow with you. Under the hood, Wipple is a powerful functional programming language with an expressive type system, but when you're just getting started, these features work invisibly. When you make a mistake, Wipple provides helpful, customizable error messages that guide you toward the solution.

## Build Wipple

The Wipple project is split across several folders. The compiler is written in [Rust](https://rust-lang.org) (via [WebAssembly](https://webassembly.org)) and [TypeScript](https://www.typescriptlang.org), and the playground is written in [React](https://react.dev). The documentation is published using [mdBook](https://github.com/rust-lang/mdBook).

You can use these commands to build and test Wipple locally. Make sure you have [`task`](https://taskfile.dev) installed.

```shell
# Compile and run './test.wipple' relative to the project directory
task dev

# View the compiled '.wippleinterface' and '.wipplelibrary' files
ls .wipple

# Run snapshot tests
task test

# Update snapshot tests
task test -- -u

# Install 'wipple' in your PATH
task cli:build

# Serve the playground at http://localhost:8080/playground
# (you will need a '.env' file in './web/playground'; see '.env.example')
task web:playground:serve

# Serve the documentation at http://localhost:8080/docs
task web:docs:serve

# Serve the full website at http://localhost:8080
task web:serve
```
