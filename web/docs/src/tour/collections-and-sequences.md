# Collections and sequences

Often, you need to store multiple values in a list. You can do this with the comma operator (`,`), which creates a `List` value by default:

```wipple
numbers : 1 , 2 , 3 , 4
```

To iterate over each item in a list, use the `each` function:

```wipple
#numbers : 1 , 2 , 3 , 4
numbers . each show
```

```wipple-output
1
2
3
4
```

`filter` lets you keep only the items that satisfy a condition:

```wipple
#numbers : 1 , 2 , 3 , 4
even? : divisible-by? 2
numbers . filter even? . each show
```

```wipple-output
2
4
```

`transform` lets you convert each item into a different item:

```wipple
#numbers : 1 , 2 , 3 , 4
double : n -> n * 2
numbers . transform double . each show
```

```wipple-output
2
4
6
8
```

You can combine `transform` and `filter` to do more complicated list processing!

```wipple
#numbers : 1 , 2 , 3 , 4
#even? : divisible-by? 2
#double : n -> n * 2
numbers
  . filter even?
  . transform double
  . each show
```

```wipple-output
4
8
```

Instead of `each`, you can use `collect` to store the final items back into a list, or another collection like `Set` or `Dictionary` if you provide a type annotation.

```wipple
#numbers : 1 , 2 , 3 , 4
#even? : divisible-by? 2
#double : n -> n * 2
doubled-evens :
  numbers
    . filter even?
    . transform double
    . collect
```

Wipple's sequencing functions are "lazy", meaning they work on one element at a time, and only once elements are requested. You can use `next` to request the next element in a sequence as a `Maybe` — if the sequence is finished, you'll get `None` back.

```wipple
#numbers : 1 , 2 , 3 , 4
#even? : divisible-by? 2
#double : n -> n * 2
-- Without `collect`, we get a lazy sequence
sequence :
  numbers
    . filter even?
    . transform double

show (next sequence)
show (next sequence)
show (next sequence)
```

```wipple-output
Some 4
Some 8
None
```

`transform`, `filter`, `each`, and `collect` are all implemented using `next`!

The laziness of sequences simplifies a lot of things — you only need to worry about one element at a time. Let's explore this now by creating our own sequence:

```wipple
count : 0
counter : sequence {
  n : count
  count! : count + 1
  Some n
}

counter
  . take 10
  . each show
```

```wipple-output
0
1
2
3
4
5
6
7
8
9
```

The `sequence` function accepts a block that's evaluated each time `next` is called. We start by initializing `count` to zero, and then incrementing it by one for each item. Remember that the block isn't evaluated until `next` is called, so we don't run into an infinite loop by continually incrementing `count`. There could be a long delay between each call to `next`, or we could take a million elements all at once! In the example above, we take just 10 elements, and then we're done.

> **Tip:** It's good practice to hide the `count` variable in a `do` block so it can't be changed accidentally outside the sequence:
>
> ```wipple
> counter : do {
>   count : 0
>   sequence {
>     n : count
>     count! : count + 1
>     Some n
>   }
> }
> ```

But wait, how can we pass a list to `transform` or `each` if we don't call `sequence` first? Wipple actually has an `As-Sequence` trait that does this for us! `List`, `Set`, `Stride`, and many other types all implement `As-Sequence`, and all the sequence functions are of the form `Collection Element where (As-Sequence Collection Element) => ...`.

Let's look at `Stride`:

```wipple
1 to 10 by 2 . each show
```

```wipple-output
1
3
5
7
9
```

Whereas a range (`min to max`) is continuous, a _stride_ (`min to max by step`) counts up in discrete steps. So it implements `As-Sequence`, and we can use it with all our sequence functions!

`Sequence` (the type returned by `sequence {...}`) implements `As-Sequence`, too — it just returns itself. That way, you can chain calls to functions like `transform` and `filter` without needing to `collect` into a list after every step.
