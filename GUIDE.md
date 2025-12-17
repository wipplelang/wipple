# Wipple Guide

Welcome to the Wipple Guide! This guide contains a tour of the Wipple programming language designed for people with an existing programming background.

## Hello, world!

To display text on the screen, use `show`:

```wipple
show "Hello, world!"
```

```
Hello, world!
```

Text is written inside double quotes (`"..."`).

`show` accepts numbers, too:

```wipple
show (1 + 2)
```

```
3
```

You need parentheses there because operators like `+` have precedence over whitespace, so `show 1 + 2` is interpreted as `(show 1) + 2`. Rather than displaying 1 on the screen and adding the result to 2, we want to add 1 to 2 and display the result. You can use parentheses like this almost anywhere!

## Variables

In Wipple, variables are defined using a colon (`:`):

```wipple
name : "Wilson"
show name

sum : 1 + 2
show sum
```

```
Wilson
3
```

You can create multiple variables with the same name. When you refer to a name in your code, the most recent variable is chosen:

```wipple
n : 1
show n

n : 2
show n
```

```
1
2
```

Each declaration is its own variable with its own value; they don't need to be the same type:

```wipple
n : 1
show n

n : "n"
show n
```

```
1
n
```

The right-hand side of the `:` is evaluated before bringing the variable into scope:

```wipple
n : 1
n : n + 1
show n
```

```
2
```

A variable can only be accessed after it's defined, not before:

```wipple
n : n + 1
n : 1
show n
```

```
example:1:5: error: can't find `n`
```

Sometimes, you need to change the value of an existing variable. You can do this by putting an exclamation mark (`!`) after the variable name:

```wipple
n : 0
n! : n + 1
show n
```

Now, any code that refers to `n` will observe the updated value.

## Blocks and control flow

A block is a piece of code surrounded in braces (`{...}`). Blocks let you store code to run it later. To run the code in a block, use `do`:

```wipple
greeting : {show "Hello, world!"}
do greeting
```

```
Hello, world!
```

Without `do`, the block will do nothing:

```wipple
greeting : {show "Hello, world!"}
-- nothing happens
```

You can call `do` on a block multiple times:

```wipple
greeting : {show "Hello, world!"}
do greeting
do greeting
```

```
Hello, world!
Hello, world!
```

You can write multiple lines of code in a block; the value of the block is the value of the last line:

```wipple
sum : {
    show "calculating 1 + 2..."
    1 + 2
}

show (do sum)
```

```
calculating 1 + 2...
3
```

You don't have to store a block in a variable before calling `do`:

```wipple
sum : do {1 + 2} -- equivalent to `sum : 1 + 2`
```

Blocks are useful for logic and control flow. For example, `if` accepts a condition and two blocks. If the condition is `True`, the first block will be evaluated, and if it's `False`, the second block will be evaluated.

```wipple
secret : 5
guess : 3
if (guess = secret) {show "You win!"} {show "Try again"}
```

```
Try again
```

`repeat` accepts a number of `times` and runs the provided block that number of times:

```wipple
repeat (3 times) {
    show "Hello, world!"
}
```

```
Hello, world!
Hello, world!
Hello, world!
```

It's important to remember that blocks are values, just like text and numbers are — they can be stored in variables and passed to functions. `repeat` is just a function that accepts a block as input. You can build your types of control flow easily, and we'll do just that in the next section!

## Functions

Functions are written with an arrow (`->`). The inputs go on the left side of the arrow, and the output goes on the right:

```wipple
add : a b -> a + b
sum : add 1 2
show sum
```

```
3
```

Functions are also just values, so they can be assigned to variables as shown above, or they can be used inline:

```wipple
sum : (a b -> a + b) 1 2
show sum
```

```
3
```

If you want to have multiple statements in a function, you can use a `do` block:

```wipple
debug-sum : a b -> do { -- don't forget `do`!
    show "called `debug-sum`"
    a + b
}

show (debug-sum 1 2)
```

```
called `debug-sum`
3
```

Let's build a function that takes a block and runs it twice:

```wipple
twice : block -> do {
    do block
    do block
}

twice {
    show "Hello, world!"
}
```

```
Hello, world!
Hello, world!
```

We just defined our own control flow!

Finally, you can use text values as functions — if you put underscore (`_`) placeholders in the text and provide values afterward, you can do string interpolation:

```wipple
greet : name -> "Greetings, _!" name
show (greet "everyone")
```

```
Greetings, everyone!
```

## Constants and types

Wipple has a powerful type system that can catch bugs in your program. By default, it works invisibly — all the code we've written so far has been fully typechecked! — but sometimes it's helpful to provide type annotations. Type annotations serve as a form of documentation, describing the kinds of values your code works with and produces.

To add a type annotation to a variable, use a double colon (`::`):

```wipple
add :: Number Number -> Number
add : a b -> a + b

show (add 1 2)
```

```
3
```

Adding a type annotation also changes how the variable is represented — rather than evaluating its value immediately, the variable is "lifted" out of the list of statements and is accessible anywhere as a constant. That means you don't have to worry about the order in which constants are defined.

```wipple
show (add 1 2)

-- Works, even though `add` is declared after the call to `show`
add :: Number Number -> Number
add : a b -> a + b
```

```
3
```

Because of this order independence, constants are "lazy": they are evaluated whenever they are used, _not_ when they are declared. You can think of the constant's value as being wrapped in a block. In practice, this isn't a problem because most constants produce functions that need to be called anyway.

Constants can also be generic — see [Type functions and traits](./type-functions-and-traits.md) for more information.

Let's look at some of the types that can be used in a type annotation:

-   `Number` is the type of numbers.
-   `Text` is the type of text.
-   `Boolean` is the type of `True` and `False`.
-   `None` is the "unit type", and is returned by functions like `show` that do something but produce no meaningful value.
-   `{A}` is the type of a block evaluating to a value of type `A`. For example, `{1 + 1}` has type `{Number}`.
-   `A -> B` is the type of a function accepting a single input of type `A` and producing a value of type `B`. Likewise, `A B C -> D` is the type of a function accepting three inputs.

You can also make your own types! We'll discuss structure types in this section, and enumeration types in the next section.

To define a structure type, pass a block of fields to `type`:

```wipple
Sport : type {
    name :: Text
    players :: Number
}
```

Any block containing only variables is assumed to be a structure value:

```wipple
basketball :: Sport
basketball : {
    name : "Basketball"
    players : 5
}
```

When you define the `Sport` type, Wipple also generates a function `Sport` that accepts the block and returns it as-is. This is useful because it allows Wipple to infer the type of the structure without needing a type annotation:

```wipple
basketball : Sport {
    name : "Basketball"
    players : 5
}
```

To get the values out of a structure, you can put a block on the left-hand side of the colon (`:`), listing the field(s)' names and the corresponding variable names.

```wipple
{name : sport-name} : basketball
show sport-name
```

```
Basketball
```

Finally, you might also see the double colon (`::`) used to annotate the type of an expression. For example, you can write:

```wipple
show (42 :: Number)
```

```
42
```

Usually this is unnecessary, but in some cases, Wipple's type inference algorithm needs help. You'll see an example of type annotations being used for this purpose in [Type functions and traits](./type-functions-and-traits.md).

## Patterns

Wipple uses pattern matching to express control flow. For example, let's say we want to generate a report card:

```wipple
Grade : type {
    A
    B
    C
    D
    F
}

report-card :: Grade -> Text
report-card : grade -> when grade {
    A -> "top of the class"
    B -> "good work"
    C -> "need to study"
    D or F -> "didn't pass"
}

show (report-card A)
```

```
top of the class
```

First, we define our patterns using `type`. Rather than providing fields, we list the _variants_, and Wipple will create an enumeration for us. Then, we use `when` to return a different value for each variant. You can use `or` to match multiple variants at once.

In fact, in Wipple, `if` is just a regular function that matches on `Boolean`. We can create our own easily:

```wipple
My-Boolean : type {
    My-True
    My-False
}

my-if : bool then else -> when bool {
    My-True -> do then
    My-False -> do else
}

show (my-if My-True {123} {456})
```

```
123
```

In addition to enumerations like these, you can store data alongside each pattern, allowing you to express values that are tied to a condition — in other words, the value is "wrapped" in a pattern, and you need to "unwrap" the value by checking for the condition using `when`. This may sound a bit confusing if you've used other languages without this feature, so let's look at an example:

```wipple
Maybe-Number : type {
    Some-Number Number
    No-Number
}
```

Here, we create a `Maybe-Number` value with two patterns. The first pattern contains a `Number`, and the second pattern contains nothing. Now, we can use pattern matching to "unwrap" the `Maybe-Number`:

```wipple
describe-maybe-number : maybe -> when maybe {
    Some-Number n -> "we have a number: _" n
    No-Number -> "we don't have a number"
}

show (describe-maybe-number (Some-Number 42))
show (describe-maybe-number No-Number)
```

```
we have a number: 42
we don't have a number
```

Why is this useful? It means we can represent errors in our program! Let's go back to our report card example, and allow the user to specify a grade as input:

```wipple
Maybe-Grade : type {
    Valid-Grade Grade
    Invalid-Grade
}

parse-grade :: Text -> Maybe-Grade
parse-grade : text -> when text {
    "A" -> Valid-Grade A
    "B" -> Valid-Grade B
    "C" -> Valid-Grade C
    "D" -> Valid-Grade D
    "F" -> Valid-Grade F
    _ -> Invalid-Grade
}

repeat forever {
    grade : parse-grade (prompt "Enter your grade")

    when grade {
        Valid-Grade g -> show (report-card g)
        Invalid-Grade -> show "invalid grade"
    }
}
```

<!-- not marked with `wipple-output` because `prompt` can't be tested -->

```
Enter your grade: A
top of the class
Enter your grade: B
good work
Enter your grade: Z
invalid grade
...
```

Wipple's type system will check for us that we handle the error — watch what happens if we pass our `Maybe-Grade` to `report-card` directly:

```wipple
grade : parse-grade (prompt "Enter your grade")
show (report-card grade)
```

```
example:32:19: error: expected `Grade` here, but found `Maybe-Grade`
```

If you've used a language with exceptions, Wipple's pattern matching is kind of like `try...catch`, but you are forced to handle every error explicitly. This can seem cumbersome at first, but it makes bugs much easier to track down. And don't worry, you don't have to define your own `Maybe` type every time — Wipple has one built in that works for any type! We'll learn how to use it in the next section.

## Type functions and traits

In the same way functions create new values from any given input, _type functions_ create new _types_ from any given input type — in other words, types can be "generic". That means we can create a `Maybe` that works for any value!

To make a type function, you use the "fat arrow" (`=>`), where the input types go on the left and the output type goes on the right:

```wipple
My-Maybe : Value => type {
    My-Some Value
    My-None
}

describe-maybe-number :: (My-Maybe Number) -> Text
describe-maybe-number : maybe -> when maybe {
    My-Some number -> "we have a number: _" number
    My-None -> "we don't have a number"
}

describe-maybe-text :: (My-Maybe Text) -> Text
describe-maybe-text : maybe -> when maybe {
    My-Some text -> "we have text: _" text
    My-None -> "we don't have text"
}

show (describe-maybe-number (My-Some 42))
show (describe-maybe-text (My-Some "Hello, world!"))
```

```
we have a number: 42
we have text: Hello, world!
```

The arrow can also be used to make constants generic, so we only need one `describe-maybe`!

```wipple
describe-maybe :: Value => (My-Maybe Value) -> Text
describe-maybe : maybe -> when maybe {
    My-Some value -> "we have a value: _" value
    My-None -> "we don't have a value"
}
```

```
example:7:22: error: cannot describe a `Value` value
```

Hmm, we run into some trouble here. The problem is that in Wipple, not all values can be converted into text! For example, let's define a `Sport` type:

```wipple
Sport : type {
    name :: Text
    players :: Number
}

basketball : Sport {
    name : "Basketball"
    players : 5
}
```

How should we display a `My-Some basketball`? Should we show the `name` first and then the number of `players`, or the other way around? What if we want to display an emoji instead? Wipple doesn't assume any particular format in which to display your custom types. What we need to do is tell Wipple how to convert `Sport` into text.

If you've used other languages, you might be familiar with the concept of an "interface"; for example, in Java, the [`Comparable`](https://docs.oracle.com/javase/8/docs/api/java/lang/Comparable.html) interface defines a `compareTo` function so you can use things like `Arrays.sort`.

Wipple has something similar called _traits_ — a trait is a container for a value that changes depending on its type. We can define a trait like so:

```wipple
My-Describe : Value => trait (Value -> Text)
```

`My-Describe` accepts a `Value` type, and produces a trait that stores a function to convert the `Value` into `Text`. Now, we can use `where` to say that `describe-maybe` is only available when `My-Describe` is implemented for `Value`:

```wipple
describe-maybe :: Value where (My-Describe Value) => (My-Maybe Value) -> Text
describe-maybe : maybe -> when maybe {
    My-Some value -> "we have a value: _" (My-Describe value)
    My-None -> "we don't have a value"
}
```

Because `My-Describe` contains a function, we can call it, passing in our `value`. Now we have a `Text` value we can use to fill in the placeholder!

Under the hood, `_` placeholders wrap the provided value in a call to the built-in `Describe` trait automatically. `Describe` is defined in the same way as our `My-Describe` trait, so from now on, we'll use `Describe` instead:

```wipple
describe-maybe :: Value where (Describe Value) => (My-Maybe Value) -> Text
describe-maybe : maybe -> when maybe {
    My-Some value -> "we have a value: _" value -- equivalent to `(Describe value)`
    My-None -> "we don't have a value"
}
```

Now, if we provide a piece of text or a number, our code works!

```wipple
show (describe-maybe (My-Some 42))
show (describe-maybe (My-Some "Hello, world!"))
```

```
we have a value: 42
we have a value: Hello, world!
```

> **Note:** We run into trouble if we provide a `My-None`. That's because Wipple can't infer for us what `Value` is supposed to be. We can fix this with a type annotation:
>
> ```wipple
> show (describe-maybe (My-None :: My-Maybe Number))
> ```
>
> ```
> we don't have a value
> ```
>
> It's rare that you'll have to do this, though.

So what about our `Sport` type from earlier? We can implement `Describe` for `Sport` using `instance`:

```wipple
instance (Describe Sport) : sport -> do {
    {
        name : name
        players : players
    } : sport

    "_ is played with _ people on each team" name players
}

show (describe-maybe (My-Some basketball))
```

```
we have a value: Basketball is played with 5 people on each team
```

`instance` can be used in place of a variable name — whenever Wipple sees an assignment to `instance`, it registers the right-hand side with the provided trait. Then, when we use `describe-maybe`, `Value` becomes `Sport`, and Wipple looks up the corresponding instance and makes it available inside `describe-maybe`.

You can also use `where` to make _instances_ conditionally available — let's replace `describe-maybe` with a custom implementation of `Describe` for our `My-Maybe` type!

```wipple
Value where (Describe Value) => instance (Describe (My-Maybe Value)) : maybe -> when maybe {
    My-Some value -> "we have a value: _" value
    My-None -> "we don't have a value"
}
```

`show` actually uses `Describe`, so now we can pass both `Sport` and `My-Maybe` values to `show`, and everything just works!

```wipple
show basketball
show (My-Some "Hello, world!")
show (My-Some basketball)
```

```
Basketball is played with 5 people on each team
we have a value: Hello, world!
we have a value: Basketball is played with 5 people on each team
```

Now that you know how type functions and traits work, you don't need to define your own `My-Maybe` type. Just use the built-in `Maybe` type, along with `Some` and `None` — it's implemented in exactly the same way you saw here!

Let's put everything together and refactor our report card program to use traits. We'll take advantage of the `Read` trait, which contains a function accepting `Text` and producing a `Maybe Value`. `prompt` uses `Read` to validate the user's input for us, so we don't need to use `repeat` either!

```wipple
Grade : type {
    A
    B
    C
    D
    F
}

report-card :: Grade -> Text
report-card : grade -> when grade {
    A -> "top of the class"
    B -> "good work"
    C -> "need to study"
    D or F -> "didn't pass"
}

-- Read : Value => trait (Text -> Maybe Value)
instance (Read Grade) : text -> when text {
    "A" -> Some A
    "B" -> Some B
    "C" -> Some C
    "D" -> Some D
    "F" -> Some F
    _ -> None
}

grade : prompt "Enter your grade"
show (report-card grade)
```

<!-- not marked with `wipple-output` because `prompt` can't be tested -->

```
Enter your grade: Z
invalid input, please try again
Enter your grade: 42
invalid input, please try again
Enter your grade: A
top of the class
```

## Modeling data

Wipple isn't an object-oriented language, but you can work with data in a similar way using structures and functions. Let's make a program that manages a bank account to explain how!

We'll start by defining our `Bank-Account` structure to hold a `balance`:

```wipple
Bank-Account : type {
    balance :: Number
}
```

Next, we'll define `deposit`:

```wipple
deposit :: Bank-Account Number -> Bank-Account
deposit : {balance : balance} amount -> {balance : balance + amount}
```

For convenience, we'll implement `Describe` as well:

```wipple
instance (Describe Bank-Account) : {balance : balance} -> "$_" balance
```

Let's try it out!

```wipple
my-account : Bank-Account {balance : 500}
show my-account

my-account : deposit my-account 100
show my-account
```

```
$500
$600
```

In Wipple, you generally create functions that return new values, rather than modifying the original. Doing it this way makes it easier to debug your code, because after you make a change, you still have the old value to compare against. In the example above, we don't care about the old bank account after we make our deposit, so we just overwrite the `my-account` variable.

There's an even better way to write the bank account example: rather than making `deposit` accept both the bank account and the amount at once, we can split the function into two. Let's try it:

```wipple
deposit :: Number -> (Bank-Account -> Bank-Account)
deposit : amount -> ({balance : balance} -> {balance : balance + amount})
```

Here, we create a function that accepts the amount to deposit, and returns _another function_ that does the actual work of updating the account. If this seems strange, keep reading!

We actually don't need the parentheses around the second function, either — the arrow reads from right to left.

```wipple
deposit :: Number -> Bank-Account -> Bank-Account
deposit : amount -> {balance : balance} -> {balance : balance + amount}
```

Now, we can split the deposit operation into two steps:

```wipple
payday : deposit 100

my-account : Bank-Account {balance : 500}
show my-account
show (payday my-account)
```

```
$500
$600
```

First, we call `deposit 100`. This returns _another function_ that accepts a `Bank-Account` and produces a new `Bank-Account` with an additional $100. We give this function a name: `payday`. Finally, we call `payday` on `my-account`, and receive the updated account.

This pattern is called _currying_, and it's useful because it lets you separate the _action_ — what you're trying to do — from the _logic_ — how the action is executed. Notice that `payday` works _independently_ of any specific bank account! The Wipple standard library uses currying in a lot of places, so it's something you'll get used to over time.

A good rule of thumb is to have the outer function accept the "data" inputs, and the inner function accept the "state" input. In object-oriented parlance, `deposit(amount)` is like a method on a `BankAccount` class.

One more thing: remember that functions can be used directly — you don't need to give them a name. So if you have an action that's difficult to name or is only used once, you can just call the inner function directly!

```wipple
my-account : Bank-Account {balance : 500}
show ((deposit 100) my-account)
```

```
$600
```

And since `(deposit 100) my-account` returns another `Bank-Account`, we can call `deposit 200` on _that_:

```wipple
my-account : Bank-Account {balance : 500}
show ((deposit 200) ((deposit 100) my-account))
```

```
$800
```

This gets unwieldy quickly, though. Fortunately, Wipple has a dot operator (`.`) for chaining function calls — `.` calls the function on the right-hand side with the input on the left-hand side.

> More formally:
>
> -   `a . f` is equivalent to `f a`,
> -   `a . f b c` is equivalent to `(f b c) a`,
> -   `a . f b . g c` is equivalent to `(g c) ((f b) a)`,
> -   and so on.

```wipple
show (my-account . deposit 100 . deposit 200)
```

```
$800
```

Although `my-account . deposit 100` looks very similar to the `myAccount.deposit(100)` method call syntax in object-oriented languages, it's important to remember that they work differently! The result of calling `deposit 100` is just a function, which you should give a name to if it makes things more clear.

> When you're reading code that uses the dot operator, it's helpful to mentally group each piece of code between the dots and think about each _action_ being performed. Then, once you understand the sequence of actions, go back to the beginning and read the input, so you know what those actions are being performed on. For example:
>
> ```wipple
> upgraded-car : car . swap engine . refill coolant . rotate tires
> ```
>
> "First, `swap` the `engine`. Then, `refill` the `coolant`. Finally, `rotate` the `tires`. Perform this maintenance on `car`, resulting in `upgraded-car`."

In the next section, we'll be using the dot operator a lot!

## Collections and sequences

Often, you need to store multiple values in a list. You can do this with the comma operator (`,`), which creates a `List` value by default:

```wipple
numbers : 1 , 2 , 3 , 4
```

To iterate over each item in a list, use the `each` function:

```wipple
numbers . each show
```

```
1
2
3
4
```

`filter` lets you keep only the items that satisfy a condition:

```wipple
even? : divisible-by? 2
numbers . filter even? . each show
```

```
2
4
```

`transform` lets you convert each item into a different item:

```wipple
double : n -> n * 2
numbers . transform double . each show
```

```
2
4
6
8
```

You can combine `transform` and `filter` to do more complicated list processing!

```wipple
numbers
  . filter even?
  . transform double
  . each show
```

```
4
8
```

Instead of `each`, you can use `collect` to store the final items back into a list, or another collection like `Set` or `Dictionary` if you provide a type annotation.

```wipple
doubled-evens :
  numbers
    . filter even?
    . transform double
    . collect
```

Wipple's sequencing functions are "lazy", meaning they work on one element at a time, and only once elements are requested. You can use `next` to request the next element in a sequence as a `Maybe` — if the sequence is finished, you'll get `None` back.

```wipple
-- Without `collect`, we get a lazy sequence
sequence :
  numbers
    . filter even?
    . transform double

show (next sequence)
show (next sequence)
show (next sequence)
```

```
Some 4
Some 8
None
```

`transform`, `filter`, `each`, and `collect` are all implemented using `next`!

The laziness of sequences simplifies a lot of things — you only need to worry about one element at a time. Let's explore this now by creating our own sequence:

```wipple
count : 0
counter : Sequence {
  n : count
  count! : count + 1
  Some n
}

counter
  . take 10
  . each show
```

```
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

The `Sequence` function accepts a block that's evaluated each time `next` is called. We start by initializing `count` to zero, and then incrementing it by one for each item. Remember that the block isn't evaluated until `next` is called, so we don't run into an infinite loop by continually incrementing `count`. There could be a long delay between each call to `next`, or we could take a million elements all at once! In the example above, we take just 10 elements, and then we're done.

> **Tip:** It's good practice to hide the `count` variable in a `do` block so it can't be changed accidentally outside the sequence:
>
> ```wipple
> counter : do {
>   count : 0
>   Sequence {
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

```
1
3
5
7
9
```

Whereas a range (`min to max`) is continuous, a _stride_ (`min to max by step`) counts up in discrete steps. So it implements `As-Sequence`, and we can use it with all our sequence functions!

`Sequence` implements `As-Sequence`, too — it just returns itself. That way, you can chain calls to functions like `transform` and `filter` without needing to `collect` into a list after every step.

## API design

Let's continue working on our bank account example! We'll start by adding an identifier to each account, so we can look up the owner.

```wipple
Bank-Account : type {
    id :: Number
    balance :: Number
}

instance (Describe Bank-Account) : {
    id : id
    balance : balance
} -> "account #_: $_" id balance
```

Then we can define an `open-account` function to build a bank account with the provided identifier:

```wipple
open-account :: Number -> Bank-Account
open-account : id -> {
    id : id
    balance : 0
}
```

Finally, we'll make a new account!

```wipple
my-account : open-account 500
show my-account
```

```
account #500: $0
```

Hmm, it looks like we made a mistake — this code seems like it's trying to open an account with $500, but instead it creates an account associated with the _identifier_ `500`. `open-account` takes a `Number` as input, but it's not clear what that number actually represents.

Let's make this API easier to understand and less error-prone by introducing a new type!

```wipple
Account-ID : type Number
```

Rather than listing the fields (for a structure) or variants (for an enumeration), when you provide a single type, Wipple creates a wrapper for you:

```wipple
-- Create an account ID wrapping 0
my-id : Account-ID 0

-- Unwrap the account ID to get the number back
Account-ID id-number : my-id
```

Let's refactor `Bank-Account` to use our new `Account-ID` type!

```wipple
Account-ID : type Number

instance (Describe Account-ID) : (Account-ID id) -> "account #_" id

Bank-Account : type {
    id :: Account-ID
    balance :: Number
}

instance (Describe Bank-Account) : {
    id : id
    balance : balance
} -> "_: $_" id balance

open-account :: Account-ID -> Bank-Account
open-account : id -> {
    id : id
    balance : 0
}
```

Now if we try to open a bank account with a plain number, we get an error:

```wipple
my-account : open-account 500
```

```
example:20:27: error: expected `Account-ID` here, but found a number
```

Great! Now it's clear what kind of data `open-account` accepts. In general, when you're designing APIs, try to create wrapper types around "plain" values like `Number` and `Text` to give the user of your API more information.

We can do the same thing for our balance, too. Another benefit of wrapper types is that you can customize how they're displayed!

```wipple
Balance : type Number

instance (Describe Balance) : (Balance dollars) -> "$_" dollars

Bank-Account : type {
    id :: Account-ID
    balance :: Balance
}

instance (Describe Bank-Account) : {
    id : id
    balance : balance
} -> "_: _" id balance
```

Next, let's implement `Add` for `Balance`, and refactor `deposit` to use it:

```wipple
instance (Add Balance Balance Balance) :
    (Balance current) (Balance amount) -> Balance (current + amount)

open-account :: Account-ID -> Bank-Account
open-account : id -> {
    id : id
    balance : Balance 0
}

deposit :: Balance -> Bank-Account -> Bank-Account
deposit : deposit -> {
    id : id
    balance : current
} -> {
    id : id
    balance : current + deposit
}

my-account : open-account (Account-ID 123)
show (my-account . deposit (Balance 50))
```

What about `withdraw`? Withdrawing is a bit trickier, since you can't withdraw more than the account's balance. Let's use `Maybe` to represent this condition — if you have enough money in your account, you get back a `Some` value, and if you try to withdraw too much, you get back `None`:

```wipple
instance (Subtract Balance Balance (Maybe Balance)) :
    (Balance current) (Balance amount) ->
        if (amount <= current) {Some (Balance (current - amount))} {None}
```

Now it's up to you how to implement `withdraw`. In this example, we'll revert back to the bank account as it was before attempting the withdrawal. This is where producing new values in Wipple, rather than mutating them in place, comes in handy!

```wipple
withdraw :: Balance -> Bank-Account -> Bank-Account
withdraw : withdrawal -> {
    id : id
    balance : current
} -> when (current - withdrawal) {
    Some new -> {
        id : id
        balance : new -- use the new balance
    }
    None -> {
        id : id
        balance : current -- revert to the balance as it was before withdrawing
    }
}

my-account : open-account (Account-ID 123)
show (my-account . withdraw (Balance 50))
```

```
account #123: $0
```

Great, now our API is designed so it's impossible to have a negative balance!

Here's the full code for our bank account API, along with documentation comments:

```wipple
-- An identifier for a bank account.
Account-ID : type Number

instance (Describe Account-ID) : (Account-ID id) -> "account #_" id

-- An amount of money stored in a bank account.
Balance : type Number

instance (Describe Balance) : (Balance dollars) -> "$_" dollars

instance (Add Balance Balance Balance) :
    (Balance current) (Balance amount) -> Balance (current + amount)

instance (Subtract Balance Balance (Maybe Balance)) :
    (Balance current) (Balance amount) ->
        if (amount <= current) {Some (Balance (current - amount))} {None}

-- A bank account.
Bank-Account : type {
    id :: Account-ID
    balance :: Balance
}

instance (Describe Bank-Account) : {
    id : id
    balance : balance
} -> "_: _" id balance

-- Open an account with the provided identifier.
open-account :: Account-ID -> Bank-Account
open-account : id -> {
    id : id
    balance : Balance 0
}

-- Deposit some money into a bank account.
deposit :: Balance -> Bank-Account -> Bank-Account
deposit : deposit -> {
    id : id
    balance : current
} -> {
    id : id
    balance : current + deposit
}

-- Attempt to withdraw some money from a bank account. If the account's balance
-- is too low, it will be left unchanged.
withdraw :: Balance -> Bank-Account -> Bank-Account
withdraw : withdrawal -> {
    id : id
    balance : current
} -> when (current - withdrawal) {
    Some new -> {
        id : id
        balance : new -- use the new balance
    }
    None -> {
        id : id
        balance : current -- revert to the balance as it was before withdrawing
    }
}
```
