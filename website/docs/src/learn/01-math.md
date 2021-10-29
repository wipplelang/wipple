# Math

To get familiar with the Wipple Playground, let's start by writing a simple calculator! Copy and paste each example into the playground using the <i class="fa fa-copy clip-button" title="Copy to clipboard" aria-label="Copy to clipboard"></i> button. Make sure to delete what's already in the editor before pasting in each new example.

Alright, here's our first example:

```wipple
2 + 2
```

...wait, nothing happened. Why not?

## Computers do exactly what you tell them to do

The problem is that you told the computer to add 2 and 2 together, but you didn't tell it what to do with the result! So the answer (`4`) is just discarded. Remember that computers always do exactly what you tell them to do, and never anything you don't. To fix this, we can add `show` to the front of our calculation, which instructs the computer to display it in the console:

```wipple
show (2 + 2)
```

Hooray, now `4` appears in the console! Try changing the numbers and experiment with other symbols, and see what happens.

## Some more examples

Copy this code into the playground! Can you guess what the output will be?

```wipple
show 1
show (1 + 1)
show (1 - 1)
show (2 * 3)
show (3 / 4)
show (0.1 + 0.2)
show (1 / 0)
```

What do you notice about writing code over multiple lines?

## More complex calculations

You can use Wipple as a full calculator by grouping calculations with parentheses. Try it out!

```wipple
show (2 + 2 + 2)
show (1 - (2 + 3))
show ((3 + 2) / 10)
show ((2 + 2) * (2 + 3 * 2))
```

<br>

OK, now that you've hopefully got the hang of the playground, let's move on to something more interesting!
