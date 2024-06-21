---
layout: blog
title: Announcing Wipple 1.0 and wipple.org
date: 2024-06-21
image: /blog/2024-06-21/images/banner.jpg
tags: [posts, featuredPosts]
blurb: I am proud to announce the release of Wipple 1.0 today! Wipple 1.0 has been four years in the making — since April 2020, I have made over 2,000 changes. Wipple 1.0 brings a new compiler, playground, and website at wipple.org, and I can't wait for you to try it out.
---

I am proud to announce the release of **Wipple 1.0** today! Wipple 1.0 has been four years in the making — since April 2020, I have made over 2,000 changes. Wipple 1.0 brings a new compiler, playground, and website at [wipple.org](https://wipple.org), and I can't wait for you to try it out.

<div style="text-align: center">
    <a href="https://www.wipple.org/playground" style="text-decoration: none">
        <strong>Try Wipple 1.0 -></strong>
    </a>
</div>

![wipple.org](/blog/2024-06-21/images/banner.jpg)

I started Wipple during high school as a way to teach programming to my friends on the robotics team. Today, it's grown into a fully-featured educational tool that I've had the opportunity to teach to students across Massachusetts. Starting in the fall, as part of my Computer Science degree at [Worcester Polytechnic Institute](https://www.wpi.edu), I will be building a curriculum for teaching computer science principles using Wipple.

Let's take a look at what's new in Wipple 1.0!

## What's new?

### New compiler

Wipple 1.0 brings a new compiler rewritten for speed and stability. It uses a build system similar to C's, where `.wipple` source files are pre-compiled into `.wippleinterface` files (which store types and signatures) and `.wipplelibrary` files (which store implementations). Basically, this means you can compile dependencies once, and build your application code against the interface file for super-fast typechecking. Then when you're ready to run your code, you link it against the library file to produce an executable.

Here's what that looks like on the command line:

```shell
$ cat pi.wipple
pi :: Number
pi : 3.14

$ cat main.wipple
show ("π = _" pi)

$ wipple compile pi.wipple \
    --interface pi.wippleinterface \
    --library pi.wipplelibrary

$ wipple compile main.wipple \
    --dependency pi.wippleinterface \
    --library main.wipplelibrary

$ wipple link pi.wipplelibrary main.wipplelibrary -o main

$ ./main
π = 3.14
```

Of course, the playground handles all of this for you automatically.

### New playground

The new Wipple Playground has been redesigned to be familiar and easier to use. It has a Google Docs-like interface, so students can navigate it quickly.

![Wipple Playground home page](/blog/2024-06-21/images/playground-home.png)

Previously, the Wipple Playground stored all your code in the URL. Now, if you sign in, your code is saved to your Google account and synced across devices!

The new playground also features an updated code editor with significantly improved syntax highlighting and support for dropdowns and other inline widgets. The Add button lets you drag commands into the editor using your mouse, so beginners can make the transition from block coding to typing more seamlessly!

![Wipple Playground code editor](/blog/2024-06-21/images/playground-editor.png)

The new Look Up button lets you quickly view documentation for a piece of code just by hovering your mouse over it...

![Wipple Playground Look Up](/blog/2024-06-21/images/playground-look-up.png)

...and clicking on the code brings up a new documentation viewer, complete with examples!

![Wipple Playground documentation](/blog/2024-06-21/images/playground-documentation.png)

### Updated lessons

There are several new lessons available in the playground, too, including a physics course!

![Physics lesson](/blog/2024-06-21/images/physics.png)

Some of the Advanced lessons are still being updated — in the meantime, you can read the [Tour of Wipple](https://www.wipple.org/docs/tour/hello-world) in the documentation.

## What's next?

Before I start building the curriculum in the fall, I plan to spend the summer thoroughly documenting the Wipple codebase so others can understand how it's organized and how everything works under the hood. The standard library also needs more documentation and examples for [Wipple by Example](https://wipple.org/playground/lesson/wipple-by-example). Finally, I've set up automated [code coverage reporting](https://app.codecov.io/gh/wipplelang/wipple), which will help inform what tests to add next.

Wipple 1.0 does not have a stability guarantee, but I will try my best to limit source-breaking changes between releases from now on.

If you'd like to track Wipple's development, please visit the [GitHub repository](https://github.com/wipplelang/wipple)!

## Conclusion

Thank you to everyone who has offered their feedback and supported me over the last four years. I'm looking forward to the next chapter of Wipple!
