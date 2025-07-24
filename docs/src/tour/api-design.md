# API design

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
#Bank-Account : type {
#    id :: Number
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {
#    id : id
#    balance : balance
#} -> "account #_: $_" id balance
open-account :: Number -> Bank-Account
open-account : id -> {
    id : id
    balance : 0
}
```

Finally, we'll make a new account!

```wipple
#Bank-Account : type {
#    id :: Number
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {
#    id : id
#    balance : balance
#} -> "account #_: $_" id balance
#
#open-account :: Number -> Bank-Account
#open-account : id -> {
#    id : id
#    balance : 0
#}
my-account : open-account 500
show my-account
```

```wipple-output
account #500: $0
```

Hmm, it looks like we made a mistake — this code seems like it's trying to open an account with $500, but instead it creates an account associated with the _identifier_ `500`. `open-account` takes a `Number` as input, but it's not clear what that number actually represents.

Let's make this API easier to understand and less error-prone by introducing a new type!

```wipple
Account-ID : type Number
```

Rather than listing the fields (for a structure) or variants (for an enumeration), when you provide a single type, Wipple creates a wrapper for you:

```wipple
#Account-ID : type Number
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
#Account-ID : type Number
#
#instance (Describe Account-ID) : (Account-ID id) -> "account #_" id
#
#Bank-Account : type {
#    id :: Account-ID
#    balance :: Number
#}
#
#instance (Describe Bank-Account) : {
#    id : id
#    balance : balance
#} -> "_: $_" id balance
#
#open-account :: Account-ID -> Bank-Account
#open-account : id -> {
#    id : id
#    balance : 0
#}
my-account : open-account 500
```

```wipple-output
example:20:27: error: expected `Account-ID` here, but found a number
```

Great! Now it's clear what kind of data `open-account` accepts. In general, when you're designing APIs, try to create wrapper types around "plain" values like `Number` and `Text` to give the user of your API more information.

We can do the same thing for our balance, too. Another benefit of wrapper types is that you can customize how they're displayed!

```wipple
#Account-ID : type Number
#
#instance (Describe Account-ID) : (Account-ID id) -> "account #_" id
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
#Account-ID : type Number
#
#instance (Describe Account-ID) : (Account-ID id) -> "account #_" id
#
#Bank-Account : type {
#    id :: Account-ID
#    balance :: Balance
#}
#
#instance (Describe Bank-Account) : {
#    id : id
#    balance : balance
#} -> "_: _" id balance
#
#Balance : type Number
#
#instance (Describe Balance) : (Balance dollars) -> "$_" dollars
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
#Balance : type Number
instance (Subtract Balance Balance (Maybe Balance)) :
    (Balance current) (Balance amount) ->
        if (amount <= current) {Some (Balance (current - amount))} {None}
```

Now it's up to you how to implement `withdraw`. In this example, we'll revert back to the bank account as it was before attempting the withdrawal. This is where producing new values in Wipple, rather than mutating them in place, comes in handy!

```wipple
#Account-ID : type Number
#
#instance (Describe Account-ID) : (Account-ID id) -> "account #_" id
#
#Bank-Account : type {
#    id :: Account-ID
#    balance :: Balance
#}
#
#instance (Describe Bank-Account) : {
#    id : id
#    balance : balance
#} -> "_: _" id balance
#
#Balance : type Number
#
#instance (Describe Balance) : (Balance dollars) -> "$_" dollars
#
#instance (Subtract Balance Balance (Maybe Balance)) :
#    (Balance current) (Balance amount) ->
#        if (amount <= current) {Some (Balance (current - amount))} {None}
#
#open-account :: Account-ID -> Bank-Account
#open-account : id -> {
#    id : id
#    balance : Balance 0
#}
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

```wipple-output
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
