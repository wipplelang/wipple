# Relations

Perhaps you've picked up the fact that "closures and modules are functions", "numbers can be added and converted to text", "traits are patterns", and so on. But wait, aren't `Function`, `Number`, `Add`, etc. all separate traits? Wipple is supposed to be strict about this!

Wipple provides a way to **derive** a trait from a value that matches a pattern. This allows you to create relations between parts of your program, like "all numbers can be added". In code, it looks like this:

```wipple
Number == Add
```

This reads as "`Number` derives `Add`", or "the `Add` trait can be derived from `Number` values". The most common relation is one that derives `Text`, to allow `show`ing values:

```wipple
Person : trait Text
name Person == Text name
```

This reads as "a value with the `Person` trait can derive the `Text` trait by using the contained `name`." In practice, it means `show`ing a person is equivalent to `show`ing the person's name:

```wipple
bob : Person "Bob"
show bob -- Bob
```

Of course, you can customize the implementation however you want!

```wipple
name Person == Text (format "_ the Great" name)

alexander : Person "Alexander"
show alexander -- Alexander the Great
```

Unlike in Python or JavaScript, the conversion from `Person` to `Text` is explicit. `show`'s parameter requires a `Text` value; if you don't pass in a `Text` value, Wipple will try to derive `Text` using the first relation that the value matches. If `Text` can't be derived, the match will fail.

> **Note:** Relations are resolved in the _opposite_ direction as variables — relations in parent scopes or declared first take precedence over relations declared in child scopes or declared later. This effectively means that once you declare a relation, it can't be overridden! Keep this in mind when you determine how specific of a pattern to match, and whether other parts of your program will need to define their own relations deriving your trait.

---

Now that we know how traits, patterns and relations work, we can make our `greet` function a bit more generic:

```wipple
Greet : trait Text -- a name to be greeted

greet : name Greet -> show (format "Hello, _!" name)

Person : trait Text -- the person's name
name Person == Greet name -- greet the person by their name

bob : Person "Bob"

Planet : trait _
Planet == Greet "world" -- always greet planets with "Hello, world!"

earth : Planet _ -- we don't need to contain anything, we just need
                 -- to know that the value is a planet

greet bob -- Hello, Bob!
greet earth -- Hello, world!
```

Creating traits that implement a function is a common pattern in Wipple. In fact, you can use the `for` function to make this task a little more concise:

```wipple
for : T Trait -> as T
```

This is a _very_ generic function, but basically it takes a trait and constructs a function that will delegate to the implementation of the trait on some value. Here's an example:

```wipple
Add : trait Function
add : for Add
```

Here, `add` is a function that accepts the input to an `Add` function, as well as the `Add` value to use in particular:

```wipple
'increment == Add (x -> x + 1)

add 2 'increment -- 3
```

This reads as "add to 2 using the implementation of `Add` defined by the name `increment`". Obviously you would almost always use a trait instead of a bare name, but it works for this simple example!

> Why does the `Add` value come after the value to be added? This is another common pattern that will come in handy once we explore functional programming in a later section.

Traits and relations form the foundation of Wipple. Now that we know how to use them, we can start writing more practical code!
