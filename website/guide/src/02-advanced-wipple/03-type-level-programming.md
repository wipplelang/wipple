# Type-level programming

Wipple's type system is so powerful, you can write programs that are executed entirely by the type checker! It turns out that values can correspond to _types_, and functions can correspond to _traits_. Let's look at a simple example of "type arithmetic":

```wipple
Z : type
S : N => type
```

Here, we define `Z` to represent zero and `S` to represent the "successor" to a number. For example, one is represented by `S Z`, two is represented by `S (S Z)`, and so on. These types are effectively the same as an enumeration at the value level:

```wipple
N : type {
    Z
    S N
}
```

> You might notice that at the type level, Wipple doesn't restrict what type you provide to `S` — you could provide `Text` or any other type. At the type level, Wipple is effectively dynamically-typed!

Now that we have our data, we can write a function to add together two numbers. This is defined using a trait:

```wipple
Add : A B Sum => trait
```

Notice that we don't have a value after the `trait` definition. This prevents the trait from being used in a value position; that is, it can only be referenced as a bound in a `where` clause. Similarly, we omit the value when we declare an `instance`:

```wipple
A => instance (Add A Z A)
```

This is our base case — any "value" `A` plus zero is equal to `A`. Our second instance is a bit more complicated:

```wipple
A B Sum where (Add A B Sum) => instance (Add A (S B) (S Sum))
```

This definition states that if you can add `A` and `B` together to get a `Sum`, then `A` plus the successor of `B` is equal to the successor of `Sum` — `a + (b + 1) = (a + b) + 1`. The recursion terminates when `B` is zero and the base case is reached. Essentially, you do the addition by repeatedly adding one to the input!

Now let's use our `Add` "function" — remember that we're working at the type level, so to perform a computation, we need to use a type annotation:

```wipple
result :: Sum where (Add (S (S Z)) (S (S (S Z))) Sum) => Sum
result : ...
```

And now we can print `result`'s type by raising a type error:

```wipple
_ : _ -> result
```

The error tells us that `Sum` is `S (S (S (S (S Z))))`, proving that 2 + 3 = 5!

```
error: could not determine the type of this expression
   ┌─ playground:11:5
   │
11 │ _ : _ -> result
   │     ^^^^^^^^^^^
   │     │
   │     try annotating the type with `::`
   │     this has type `a -> S (S (S (S (S Z))))` for some unknown type `a`
```

Let's try another example — determining if a number is odd or even! We'll start by defining our data:

```wipple
Z : type
S : N => type

Odd : type
Even : type
```

And now we'll make our function, which accepts a number `N` and returns a kind `Kind` (`Odd` or `Even`):

```wipple
Odd-Even : N Kind => trait
```

Our base case is that `Zero` is `Even`:

```wipple
instance (Odd-Even Z Even)
```

And now we implement the recursion! If `n + 1` is odd, then `n` is even, and vice versa:

```wipple
N where (Odd-Even N Even) => instance (Odd-Even (S N) Odd)
N where (Odd-Even N Odd) => instance (Odd-Even (S N) Even)
```

Let's try it out!

```wipple
zero :: A where (Odd-Even Z A) => A
zero : ...
_ : _ -> zero

one :: A where (Odd-Even (S Z) A) => A
one : ...
_ : _ -> one

two :: A where (Odd-Even (S (S Z)) A) => A
two : ...
_ : _ -> two
```

Sure enough, zero is even, one is odd, and two is even:

```
error: could not determine the type of this expression
   ┌─ playground:14:5
   │
14 │ _ : _ -> zero
   │     ^^^^^^^^^
   │     │
   │     try annotating the type with `::`
   │     this has type `a -> Even` for some unknown type `a`

error: could not determine the type of this expression
   ┌─ playground:18:5
   │
18 │ _ : _ -> one
   │     ^^^^^^^^
   │     │
   │     try annotating the type with `::`
   │     this has type `a -> Odd` for some unknown type `a`

error: could not determine the type of this expression
   ┌─ playground:22:5
   │
22 │ _ : _ -> two
   │     ^^^^^^^^
   │     │
   │     try annotating the type with `::`
   │     this has type `a -> Even` for some unknown type `a`
```

## A practical example: `first` and `rest`

At the value level, you can use the `first` and `rest` functions to obtain the first item in a list or the items after it. These functions return `Maybe` values, since the input list could be empty. But at the type level, we can ensure that the list is non-empty! To start, let's create a new list type along with a `nil` constructor:

```wipple
Z : type
S : N => type

List : A Count => type -- no need to store an `A` because we don't care about
                       -- the list's value

nil :: A => List A Z
nil : ...
```

And now we can implement `first` like so:

```wipple
first :: A Count => List A (S Count) -> A
first : ...
```

Notice that `first` requires that the list's `Count` be the successor of a number; this is effectively saying that `Count` must be greater than zero. `rest` is similar:

```wipple
rest :: A Count => List A (S Count) -> List A Count
rest : ...
```

Here, we make sure to reduce the `Count` by one in the returned list.

And lastly, let's implement `cons`:

```wipple
cons :: A Count => A -> List A Count -> List A (S Count)
cons : ...
```

Now let's test our program!

```wipple
my-list : cons 1 (cons 2 (cons 3 nil))
first my-list -- works!
first (rest my-list) -- works!
first (rest (rest (rest my-list))) -- fails!
```

The last line fails with a type error:

```
error: mismatched types
   ┌─ playground:22:7
   │
22 │ first (rest (rest (rest my-list))) -- fails!
   │       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ expected `List a (S b)` for some unknown types `b` and `a`, but found `List Number Z`
```

Awesome, now we have compile-time bounds checking!
