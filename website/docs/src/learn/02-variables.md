# Variables

You're probably familiar with variables in math — a variable is just a way to give some complex calculation a name. It's the same thing in Wipple! Let's try it out:

```wipple
x : 2 + 2
show x
```

Instead of using an `=` sign, Wipple uses `:` (a colon). You can pronounce `a : b` as _"a is b"_. Now, anywhere the computer sees `sum`, it will replace it with `2 + 2`!

Let's try a more complicated example. Can you guess what the output will be?

```wipple
a : 3
b : a + 2
c : a + b
show c
```

<br>

Note that unlike in math, you can use words for variables in Wipple. Like this:

```wipple
sum : 2 + 2
show sum

my-awesome-variable : 3 + 4
show my-awesome-variable
```

Try making some variables of your own in the playground!

<br>

Can you guess what this code does?

```wipple
show hmmm
```

No output! In fact, you should notice a red squiggle underneath `hmmm`. Try to fix this error — what did you have to do? (Hint: hover over the red squiggle to see more information about the error!)
