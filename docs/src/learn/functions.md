# Functions

One way to declare functions in Wipple is with a closure. To declare a closure, put an arrow (`->`, pronounced _becomes_) between the parameter and the return value:

```wipple
increment : x -> x + 1
```

To call a function, write the name of the function and then its input:

```wipple
increment 2 -- returns 3
```

Functions can only accept one parameter. To accept multiple parameters, use multiple functions!

```wipple
-- a function that returns another function that returns the sum
add : a -> b -> a + b
```

Place all the inputs one after another, like so. They will be reduced into the final value:

```wipple
add 2 3 -- returns 5

add-four : a -> b -> c -> d -> a + b + c + d
add-four 10 9 8 7 -- returns 34
```

You can also "partially apply" a function, meaning you don't give all the inputs right away:

```wipple
increment : add 1 -- 'add 1' returns a function which we can store
increment 100 -- returns 101
```

Finally, you can use parentheses to group function calls into a single value:

```wipple
show (add 2 3) -- shows 5 on the right
```

Without the parentheses, `show` would get `add` as its input, and then an error would occur when trying to also pass in `2` and `3`.
