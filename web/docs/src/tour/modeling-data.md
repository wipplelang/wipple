# Modeling data

Wipple isn't an object-oriented language, but you can work with data in a similar way using structures and functions. Let's make a program that manages a bank account to explain how!

We'll start by defining our `Bank-Account` structure to hold a `balance`:

```wipple
Bank-Account : type {
    balance :: Number
}
```

Next, we'll define `deposit`:

```wipple
#Bank-Account : type {
#    balance :: Number
#}
deposit :: Bank-Account Number -> Bank-Account
deposit : {balance : balance} amount -> {balance : balance + amount}
```

For convenience, we'll implement `Describe` as well:

```wipple
#Bank-Account : type {
#    balance :: Number
#}
instance (Describe Bank-Account) : {balance : balance} -> "$_" balance
```

Let's try it out!

```wipple
#Bank-Account : type {
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {balance : balance} -> "$_" balance
#
#deposit :: Bank-Account Number -> Bank-Account
#deposit : {balance : balance} amount -> {balance : balance + amount}
my-account : Bank-Account {balance : 500}
show my-account

my-account : deposit my-account 100
show my-account
```

```wipple-output
$500
$600
```

In Wipple, you generally create functions that return new values, rather than modifying the original. Doing it this way makes it easier to debug your code, because after you make a change, you still have the old value to compare against. In the example above, we don't care about the old bank account after we make our deposit, so we just overwrite the `my-account` variable.

There's an even better way to write the bank account example: rather than making `deposit` accept both the bank account and the amount at once, we can split the function into two. Let's try it:

```wipple
#Bank-Account : type {
#    balance :: Number
#}
deposit :: Number -> (Bank-Account -> Bank-Account)
deposit : amount -> ({balance : balance} -> {balance : balance + amount})
```

Here, we create a function that accepts the amount to deposit, and returns _another function_ that does the actual work of updating the account. If this seems strange, keep reading!

We actually don't need the parentheses around the second function, either — the arrow reads from right to left.

```wipple
#Bank-Account : type {
#    balance :: Number
#}
deposit :: Number -> Bank-Account -> Bank-Account
deposit : amount -> {balance : balance} -> {balance : balance + amount}
```

Now, we can split the deposit operation into two steps:

```wipple
#Bank-Account : type {
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {balance : balance} -> "$_" balance
#
#deposit :: Number -> Bank-Account -> Bank-Account
#deposit : amount -> {balance : balance} -> {balance : balance + amount}
payday : deposit 100

my-account : Bank-Account {balance : 500}
show my-account
show (payday my-account)
```

```wipple-output
$500
$600
```

First, we call `deposit 100`. This returns _another function_ that accepts a `Bank-Account` and produces a new `Bank-Account` with an additional $100. We give this function a name: `payday`. Finally, we call `payday` on `my-account`, and receive the updated account.

This pattern is called _currying_, and it's useful because it lets you separate the _action_ — what you're trying to do — from the _logic_ — how the action is executed. Notice that `payday` works _independently_ of any specific bank account! The Wipple standard library uses currying in a lot of places, so it's something you'll get used to over time.

A good rule of thumb is to have the outer function accept the "data" inputs, and the inner function accept the "state" input. In object-oriented parlance, `deposit(amount)` is like a method on a `BankAccount` class.

One more thing: remember that functions can be used directly — you don't need to give them a name. So if you have an action that's difficult to name or is only used once, you can just call the inner function directly!

```wipple
#Bank-Account : type {
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {balance : balance} -> "$_" balance
#
#deposit :: Number -> Bank-Account -> Bank-Account
#deposit : amount -> {balance : balance} -> {balance : balance + amount}
my-account : Bank-Account {balance : 500}
show ((deposit 100) my-account)
```

```wipple-output
$600
```

And since `(deposit 100) my-account` returns another `Bank-Account`, we can call `deposit 200` on _that_:

```wipple
#Bank-Account : type {
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {balance : balance} -> "$_" balance
#
#deposit :: Number -> Bank-Account -> Bank-Account
#deposit : amount -> {balance : balance} -> {balance : balance + amount}
my-account : Bank-Account {balance : 500}
show ((deposit 200) ((deposit 100) my-account))
```

```wipple-output
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
#Bank-Account : type {
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {balance : balance} -> "$_" balance
#
#deposit :: Number -> Bank-Account -> Bank-Account
#deposit : amount -> {balance : balance} -> {balance : balance + amount}
#my-account : Bank-Account {balance : 500}
show (my-account . deposit 100 . deposit 200)
```

```wipple-output
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

In the next chapter, we'll be using the dot operator a lot!
