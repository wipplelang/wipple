<p align="center">
  <a href="https://wipple.org">
    <img src="web/home/images/logo.svg">
  </a>
</p>

<h1 align="center">
  The Wipple Programming Language
</h1>

<p align="center">
  <a href="https://wipple.org/docs">
    <img src="https://img.shields.io/badge/docs-wipple.org%2Fdocs-blue">
  </a>
  <a href="https://github.com/wipplelang/wipple/actions/workflows/test.yml">
    <img src="https://github.com/wipplelang/wipple/actions/workflows/test.yml/badge.svg?branch=main" alt=".github/workflows/test.yml">
  </a>
  <a href="https://codecov.io/gh/wipplelang/wipple" >
    <img src="https://codecov.io/gh/wipplelang/wipple/branch/main/graph/badge.svg?token=45CTMYY78V"/>
  </a>
  <a href="https://app.netlify.com/sites/wipple/deploys">
    <img src="https://api.netlify.com/api/v1/badges/e72cbb94-f8a4-4e15-8769-11d51bbae200/deploy-status" alt="Netlify Status">
  </a>
<p>

Wipple is an educational programming language designed by [Wilson Gramer](https://gramer.dev). With Wipple, you can make art and music, explore math and science, design video games, and more.

You write Wipple code in the [Wipple Playground](https://wipple.org/playground), an interactive, collaborative, online coding platform. The playground works on any device with no setup required. With 80+ lessons and guides, there’s something for everyone.

Wipple code is natural to read and write, and the language is designed to grow with you. Under the hood, Wipple is a powerful functional programming language with an expressive type system, but when you're just getting started, these features work invisibly. When you make a mistake, Wipple provides helpful, customizable error messages that guide you toward the solution.

## Build Wipple

The Wipple project is split across several folders. The compiler is written in [Rust](https://rust-lang.org) (via [WebAssembly](https://webassembly.org)), and the playground is written in [React](https://react.dev). The documentation is published using [mdBook](https://github.com/rust-lang/mdBook).

You can use these commands to build and test Wipple locally. Make sure you have [`task`](https://taskfile.dev) installed.

```shell
# Build and install the compiler and VSCode extension from source
task install

# Run tests
task test

# Serve the website at http://localhost:8080
# (you will need a '.env' file in 'web'; see '.env.example')
task web:serve
```

Once you have `wipple` in your `PATH`, you can run an example program with these commands:

```
cd example
echo 'show "Hello, world!"' > src/test.wipple
wipple run
```

You should see `Hello, world!` in the console.
