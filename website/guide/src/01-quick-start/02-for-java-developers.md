# Quick start for Java developers

Welcome to Wipple! This guide goes over some basic Java concepts and their equivalent in Wipple. When you finish this guide, you’ll have a foundational understanding of Wipple code that you can use to experiment on your own.

## Hello, world

Wipple's equivalent of `System.out.println` is `show`:

```wipple
show "Hello, world!"
```

Notice that there's no semicolons in Wipple code — just put each statement on its own line.

## Comments, numbers and strings

You can write a comment using `--`. Wipple only has line comments:

```wipple
-- This is a comment
this is executed -- this is not
```

Numbers are represented in base 10 instead of floating point by default, but they are written the same way:

```wipple
42
3.14
-1
```

Strings are called "text" in Wipple:

```wipple
"Hello, world!"
"line 1\nline 2"
```

You can use `format` to do string interpolation. All of the `_`s will be replaced by the string representation of the provided values:

```wipple
format "Hello, _!" "world" -- Hello, world!
```

## Variables

In Wipple, you can declare variables using the `:` operator:

```wipple
answer : 42
name : "Wipple"
```

Wipple has type inference, so you don't need to write the type of the variable — Wipple will infer it automatically! If you really want to declare the type, you can do so using the `::` operator:

```wipple
answer : (42 :: Number)
name : ("Wipple" :: Text)
```

Alternatively, you can write the type on its own line just above the variable declaration:

```wipple
answer :: Number
answer : 42

name :: Text
name : "Wipple"
```

> **Note:** This syntax actually transforms the variable into a constant that's lazily evaluated. It's primarily intended for use in libraries and not in the bodies of functions, top-level code, or other places where the evaluation order matters. The separate-line syntax is required if you want to use generics or recursion.

All variables in Wipple are the equivalent of `final` — you can't change their value after declaring them. If you need access to mutable state, you can do so using `mutable`. By convention, functions that change a mutable value end in `!`.

```wipple
counter : mutable (0 :: Natural)

show (get counter) -- 0

increment! counter
show (get counter) -- 1
```

## `if` statement

Wipple's `if x a b` is equivalent to Java's `x ? a : b`:

```wipple
password : "letmein123"
valid : password = "password123!" -- use a single '=' to compare values
show (if valid "Access granted" "Access denied") -- Access denied
```

Note that Wipple doesn't have an `if` statement. If you want to execute multiple statements inside an `if`, either refactor the code to use functions or use a block expression:

```wipple
-- This is OK...
if (1 + 1 = 2) {
    show "Woohoo!"
} {
    show "Oh no"
}

-- But this is better...
result : if (1 + 1 = 2) "Woohoo!" "Oh no"
show result
```

## Basic types

Wipple is a statically-typed language just like Java. However, Wipple has type inference, so you usually don't need to think about types at all! As mentioned above, you can use `::` to annotate the type of a value:

```wipple
42 :: Number
"Hello" :: Text
```

If you mismatch the types, Wipple will emit an error:

```wipple
42 :: Text -- mismatched types: expected `Text`, but found `Number`
```

## Classes and objects

Wipple calls classes "types", which you can create using `type`:

```wipple
Person : type {
    name :: Text
    age :: Number
}
```

Instead of defining a constructor, in Wipple you instantiate a type by writing the name of the type followed by its fields:

```wipple
bob : Person {
    name : "Bob"
    age : 35
}
```

And instead of `bob.name` and `bob.age`, you can use destructuring or the `of` operator:

```wipple
-- Preferred way
{ name age } : bob

-- Alternative way
name : name of bob
age : age of bob
```

## Functions

Wipple's functions work like Java's lambda expressions. In fact, they both use the arrow notation!

```wipple
increment : x -> x + 1
show (increment 42) -- 43
```

One big difference is that Wipple functions may only accept a single parameter. If you want multiple parameters, use multiple functions!

```wipple
add : a -> b -> a + b
show (add 1 2) -- 3
```

If that's confusing, here's the equivalent Java code:

```java
Function<Double, Function<Double, Double>> add = a -> b -> a + b;
System.out.println(add.apply(1.0).apply(2.0)); // 3.0
```

## Methods

Wipple doesn't allow you to add methods to an object (although you can store functions inside types like any other value). Instead, you can declare functions like this:

```wipple
greet :: Person -> Text
greet : { name } -> format "Hello, _!" name

greet bob -- Hello, Bob!
```

Alternatively, you can use the `.` operator to chain function calls:

```wipple
bob . greet -- Hello, Bob!
```

## Interfaces

Wipple has something similar to interfaces called "traits". For example, here is how you would define a `Greet` trait and implement it for `Person` and `Earth`:

### Java

```java
// Greet is an interface that can be implemented with a function returning text
interface Greet {
    String greet();
}

class Greeter {
    // For any value implementing Greet, return a greeting
    static <A extends Greet> String greet(A x) {
        return "Hello, " + x.greet() + "!";
    }
}

class Person implements Greet {
    String name;

    Person(String name) {
        this.name = name;
    }

    // Greet for Person values is defined as the person's name
    public String greet() {
        return this.name;
    }
}

class Earth implements Greet {
    // Greet for Earth values is defined as "world"
    public String greet() {
        return "world";
    }
}

class Main {
    public static void main(String[] args) {
        System.out.println(Greeter.greet(new Person("Bob"))); // Hello, Bob!
        System.out.println(Greeter.greet(new Earth())); // Hello, world!
    }
}
```

### Wipple

```wipple
-- Greet is a trait that can be defined with a function returning text
Greet : A => trait (A -> Text)

-- For any value where Greet is defined, return a greeting
greet :: A where (Greet A) => A -> Text
greet : x -> format "Hello, _!" (Greet x)


Person : type {
    name :: Text
}

-- Greet for Person values is defined as the person's name
instance (Greet Person) : { name } -> name


Earth : type

-- Greet for Earth values is defined as "world"
instance (Greet Earth) : just "world"


show (greet (Person { name : "Bob" })) -- Hello, Bob!
show (greet Earth) -- Hello, world!
```

Built-in Java methods like `equals` and `toString` are also implemented using traits in Wipple. For example:

```wipple
Person : type {
    name :: Text
    age :: Number
}

instance (Equal Person) : p1 -> p2 ->
    name of p1 = name of p2
        and age of p1 = age of p2

instance (Show Person) : { name age } ->
    format "_ is _ years old" name age
```

And now we can `show` a `Person` value!

```wipple
bob : Person {
    name : "Bob"
    age : 30
}

show bob -- Bob is 30 years old
```

Wipple also allows you to derive traits like `Equal` automatically!

```wipple
instance Equal Person -- auto-generates an implementation
```

## Inheritance

Wipple is not an object-oriented language and doesn't support class inheritance. But you can achieve the same functionality using composition and traits! For example, let's write some Java code for a GUI application:

```java
class Button extends View {
    String title;

    Button(String title) {
        this.title = title;
    }

    void draw(Window window) {
        window.drawText(this.title);
    }
}

class RedButton extends Button {
    @Override
    void draw(Window window) {
        window.setColor(Color.RED);
        super.draw(window);
    }
}

class BlueButton extends Button {
    @Override
    void draw(Window window) {
        window.setColor(Color.BLUE);
        super.draw(window);
    }
}
```

Let's refactor this code in Wipple to use composition instead of inheritance:

```wipple
Button : type {
    title :: Text
    color :: Color
}

instance (View Button) : window -> { title color } ->
    window
        . set-color color
        . draw-text title


Red-Button : type {
    title :: Text
}

instance (View Red-Button) : window -> { title } -> {
    button : Button {
        title
        color : Red
    }

    button . View window
}


Blue-Button : type {
    title :: Text
}

instance (View Blue-Button) : window -> { title } -> {
    button : Button {
        title
        color : Blue
    }

    button . View window
}
```

Instead of calling `super`, you can delegate to the `View` implementation of `Button` inside the implementations of `Red-Button` and `Blue-Button`.

For this particular example, it makes more sense to create a constructor function instead of a whole new type for each color of button:

```wipple
Button : type {
    title :: Text
    color :: Color
}

instance (View Button) : window -> { title color } ->
    window
        . set-color color
        . draw-text title


red-button :: Text -> Button
red-button : title -> Button {
    title
    color : Red
}


blue-button :: Text -> Button
blue-button : title -> Button {
    title
    color : Blue
}
```

## Handling `null`

In Java, you represent the absence of a value using `null`. Wipple has something similar called `None`, but Wipple helps ensure that you handle `None` throughout your program. It achieves this using a `Maybe` type, which holds `Some x` (for any `x`) or `None`. Imagine a Java program that fetches a user from a database:

```java
class Database {
    public User fetchUser(int id) {
        Table table = this.table("users");

        if (table.contains(id)) {
            return table.get(id);
        } else {
            return null;
        }
    }
}

class Main {
    public static void main(String[] args) {
        Database database = ...;
        User bob = database.fetchUser(42);
        System.out.println(bob.name);
    }
}
```

Uh oh, this program has a bug — we forgot to handle the case where `fetchUser` returns `null`! Now let's write the same program in Wipple:

```wipple
fetch-user :: Integer -> Database -> Maybe User
fetch-user : id -> database -> {
    table : database . table "users"

    if (table . contains? id)
        (Some (table . get id))
        None
}


database : ...

bob : database . fetch-user 42
show bob
```

Now we get the following error:

```
error: missing instance
   ┌─ playground:11:1
   │
11 │ show bob
   │ ^^^^ could not find instance `Show (Maybe User)`
   │
   ┌─ https://pkg.wipple.dev/std/output.wpl:16:17
   │
16 │ show :: A where (Show A) => A -> ()
   │                 -------- required by this bound here
```

As you can see, Wipple doesn't know how to `show` a `Maybe User`. We have to handle the case where the user is `None`! In Wipple, we can accomplish handle the `None` case using `when`:

```wipple
when (database . fetch-user 42) {
    Some bob -> show bob
    None -> show "error: no such user"
}
```

Great — now our code won't crash and we can choose how to handle the error in an application-specific way!

## Exceptions

Wipple doesn't have exceptions. Instead, functions that can produce errors return the `Result` type. Similar to `Maybe`, `Result` stores either an `OK x` or an `Error e`. Let's refactor our `fetch-user` example to return a `Result` instead of a `Maybe`:

```wipple
Database-Error : type {
    message :: Text
}

instance (Show Database-Error) : { message } -> format "database error: _" message


fetch-user :: Integer -> Database -> Result User Database-Error
fetch-user : id -> database -> {
    table : database . table "users"

    if (table . contains? id)
        (OK (table . get id))
        (Error (Database-Error { message : format "no user with id _" id }))
}
```

And we can handle the error using `when`:

```wipple
when (database . fetch-user 42) {
    OK bob -> show bob
    Error error -> show error
}
```

To propagate the error up, you can use `end`:

```wipple
bob : when (database . fetch-user 42) {
    OK user -> user
    Error error -> end (Error error)
}

show bob
```

This pattern can be written more succinctly using `try`:

```wipple
bob : try (database . fetch-user 42)
show bob
```
