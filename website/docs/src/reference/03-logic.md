# Logic

Wipple's fundamental logic operation is `when`, which executes a given path based on the input value:

```wipple
Grade : enum (A B C D F)
use Grade

Grade is Show : grade -> when grade {
    A => "Top of the class"
    B => "Pretty good"
    C => "Getting there"
    D or F => "Need to study"
}
```

Wipple doesn't have regular booleans! Boolean logic is implemented using the `Boolean` enum:

```wipple
Boolean : enum (True False)
use Boolean

if : template bool then else -> when bool {
    True  => then
    False => else
}

show (format "x _ 5" (if (x = 5) "is" "is not"))
```

You can bind variables inside `when` like so:

```wipple
Maybe : for A -> enum {
    Some A
    None
}

use Maybe

Maybe is Map : f -> x? -> when x? {
    Some x => Some (f x)
    None => None
}
```

The first expression before a `=>` is always a variant of an enum, and any subsequent names are bound to the respective values. You can bind inner patterns using `:`, and you can bind multiple variables in the same condition, too:

```wipple
area : when shape {
    Square s => s ^ 2
    Rectangle l w => l * w
    Circle r => 3.14 * r ^ 2
}
```

The cases you provide to `when` must be exhaustive (ie. they must cover all variants of the enum). To ignore some cases, or to handle multiple cases in one branch, you can use `?` and `or`:

```wipple
when color? {
    Some (Red or Blue) => "hooray"
    ? => "oh no"
}
```

Alternatively, you can use `let`:

```wipple
let color? (Some (Red or Blue)) (show "hooray")
```

If you just want to execute a piece of code when a condition is true, you can pass an expression to `when` instead of a block of cases:

```wipple
when (2 + 2 = 4) (show "woohoo")
```

The opposite form is `unless`:

```wipple
unless : template bool body -> when (not bool) body
```

Finally, you can also assign to a pattern. If the pattern doesn't match, the program will crash:

```wipple
x? : Some 42
Some x : x? -- obtain the 'Some' value within 'x?'

x? : None
Some x : x? -- runtime error
```
