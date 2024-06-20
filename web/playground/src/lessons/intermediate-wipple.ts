import { Lesson } from "./index";

export const intermediateWippleLesson: Lesson = {
    id: "intermediate-wipple",
    name: "Intermediate Wipple",
    description:
        "Explore how to use functions, patterns, and types to write more complex programs.",
    pages: [
        {
            items: [
                {
                    text: "## Functions\n\nIn Wipple, a **function** is a way to take a piece of code and use it multiple times without having to copy-paste the code over and over again. For example, let's say we wanted to make a program that displays the sports you can play:",
                    locked: true,
                    type: "text",
                },
                {
                    code: 'basketball : "basketball"\nplays-basketball : "I can play _!" basketball\nshow plays-basketball\n\nsoccer : "soccer"\nplays-soccer : "I can play _!" soccer\nshow plays-soccer\n\ngolf : "golf"\nplays-golf : "I can play _!" golf\nshow plays-golf',
                    type: "code",
                },
                {
                    locked: true,
                    type: "text",
                    text: "This code is getting a bit repetitive, but it has a pattern — the text only differs by the name of the sport. That means we can make a function that accepts a sport and displays the text!",
                },
                {
                    code: 'play : sport -> show ("I can play _!" sport)\n\nplay "basketball"\nplay "soccer"\nplay "golf"',
                    type: "code",
                },
                {
                    locked: true,
                    type: "text",
                    text: "As you can see, functions are written using an arrow (`->`, a dash followed by a greater-than sign). The input is on the left, and the output is on the right.\n\nFunctions can have a single input or multiple inputs. Just list them all before the arrow:",
                },
                {
                    code: "add : a b -> a + b\nsum : add 1 2\nshow sum",
                    type: "code",
                },
                {
                    type: "text",
                    text: "Let's go back to turtle graphics and add some functions. We can make a `draw-square` function whose input is `size`, and now we can draw a square of any size!",
                    locked: true,
                },
                {
                    type: "code",
                    code: "draw-square : size -> repeat (4 times) {\n  forward size\n  left (90 degrees)\n}\n\nspeed very-fast\ndraw-square (10 pixels)\ndraw-square (20 pixels)\ndraw-square (30 pixels)\ndraw-square (40 pixels)\ndraw-square (50 pixels)",
                    setup: "turtle",
                },
                {
                    text: "Learning how to use functions is one of the most important skills you can have as a programmer. Over time, you’ll figure out when it’s a good idea to split your code into functions. For now, though, a good rule of thumb is that any time you have multiple pieces of code that do the same thing, you can make your code easier to read and change by using functions instead. The majority of the remaining Learn Wipple lessons are focused on making your code less repetitive by using and composing functions!\n\nNow for something fun… in the next lesson!",
                    locked: true,
                    type: "text",
                },
            ],
            name: "Functions",
            id: "7Jb5ygnNVsrd8Sw9FDwB",
        },
        {
            id: "mO-I1D3yXqN2CTrIBhn1",
            name: "Making music",
            items: [
                {
                    text: "## Making music\n\nLet’s look at how we can use Wipple to make music! If we create a Music program, we get access to `note`:",
                    type: "text",
                    locked: true,
                },
                {
                    type: "code",
                    setup: "music",
                    code: 'note [Note "C4"] (4 beats)',
                },
                {
                    text: "Press Play and you should hear a note!\n\nTo add another note, press the **Add** button in the top left of the code editor and drag the piano icon into your code. Click on the piano icon to bring up a keyboard, and click on a key to change the note.\n\nTo play music on a particular instrument, use `instrument` and drag in an instrument name from the **Add** menu. You can look at the list of instruments by clicking on the instrument in the box.",
                    locked: true,
                    type: "text",
                },
                {
                    code: 'instrument [Instrument-Name "orchestra_hit"] {\n  note [Note "C4"] (4 beats)\n}',
                    setup: "music",
                    type: "code",
                },
                {
                    locked: true,
                    type: "text",
                    text: "You can use `repeat` to make repeating sections of music, and you can use `chord` to play a list of notes at the same time:",
                },
                {
                    code: 'tempo 140\n\ninstrument [Instrument-Name "marimba"] {\n  repeat (4 times) {\n    chord ([Note "C5"] , [Note "F4"]) ((1 / 4) beats)\n    note [Note "Bb4"] ((1 / 4) beats)\n    note [Note "G4"] ((1 / 2) beats)\n    note [Note "C5"] ((1 / 2) beats)\n    chord ([Note "F4"] , [Note "Bb3"]) ((1 / 2) beats)\n    note [Note "C5"] ((1 / 2) beats)\n    note [Note "Bb4"] ((1 / 2) beats)\n    note [Note "C5"] ((1 / 2) beats)\n    chord ([Note "F4"] , [Note "A3"]) ((1 / 2) beats)\n    rest ((3 / 2) beats)\n    chord ([Note "G4"] , [Note "Eb4"]) (1 beats)\n    chord ([Note "G4"] , [Note "C4"]) ((1 / 2) beats)\n    chord ([Note "Bb4"] , [Note "Eb4"]) ((1 / 2) beats)\n    chord ([Note "C5"] , [Note "F4"]) ((1 / 2) beats)\n  }\n}',
                    setup: "music",
                    type: "code",
                },
                {
                    locked: true,
                    text: "You can have multiple instruments and they will all play simultaneously, so you can make your own orchestra!",
                    type: "text",
                },
                {
                    type: "code",
                    code: 'tempo 144\n\ninstrument [Instrument-Name "violin"] {\n  note [Note "G4"] ((1 / 2) beats)\n  note [Note "G4"] ((1 / 2) beats)\n  note [Note "G4"] ((1 / 2) beats)\n  note [Note "Eb4"] (4 beats)\n  rest (2 beats)\n\n  note [Note "F4"] ((1 / 2) beats)\n  note [Note "F4"] ((1 / 2) beats)\n  note [Note "F4"] ((1 / 2) beats)\n  note [Note "D4"] (4 beats)\n}\n\ninstrument [Instrument-Name "viola"] {\n  note [Note "G3"] ((1 / 2) beats)\n  note [Note "G3"] ((1 / 2) beats)\n  note [Note "G3"] ((1 / 2) beats)\n  note [Note "Eb3"] (4 beats)\n  rest (2 beats)\n\n  note [Note "F3"] ((1 / 2) beats)\n  note [Note "F3"] ((1 / 2) beats)\n  note [Note "F3"] ((1 / 2) beats)\n  note [Note "D3"] (4 beats)\n}\n\ninstrument [Instrument-Name "cello"] {\n  note [Note "G2"] ((1 / 2) beats)\n  note [Note "G2"] ((1 / 2) beats)\n  note [Note "G2"] ((1 / 2) beats)\n  note [Note "Eb2"] (4 beats)\n  rest (2 beats)\n\n  note [Note "F2"] ((1 / 2) beats)\n  note [Note "F2"] ((1 / 2) beats)\n  note [Note "F2"] ((1 / 2) beats)\n  note [Note "D2"] (4 beats)\n}\n\ninstrument [Instrument-Name "contrabass"] {\n  note [Note "G2"] ((1 / 2) beats)\n  note [Note "G2"] ((1 / 2) beats)\n  note [Note "G2"] ((1 / 2) beats)\n  note [Note "Eb2"] (4 beats)\n  rest (2 beats)\n\n  note [Note "F2"] ((1 / 2) beats)\n  note [Note "F2"] ((1 / 2) beats)\n  note [Note "F2"] ((1 / 2) beats)\n  note [Note "D2"] (4 beats)\n}',
                    setup: "music",
                },
            ],
        },
        {
            name: "The type system",
            id: "neXLAyR8KGxubEvFrEQl",
            items: [
                {
                    locked: true,
                    type: "text",
                    text: "## The type system\n\nUnderlying the Wipple programming language is an advanced algorithm called the **type system**. Wipple uses the type system to determine what you mean, and that influences how the code is run. For example, if we `prompt` the user for a number...",
                },
                {
                    code: 'n : prompt "Enter a number"\nshow ("The next number after _ is _" n (n + 1))',
                    type: "code",
                },
                {
                    locked: true,
                    type: "text",
                    text: '...you\'ll notice that we can only type numbers into the text box, because it only makes sense to add `1` to a number. So how does it _know?_\n\nEssentially, the type system works by giving every piece of code a **type**, which represents the "kind of thing" that code produces. Types begin with a capital letter, and in the same way variables are denoted with a single colon `:`, types are denoted with a double colon `::`. So we can say that `1` is a `Number`, or `1 :: Number`. Likewise, `"hello" :: Text`.\n\nLet\'s try making a mistake and see if Wipple catches it:',
                },
                {
                    type: "code",
                    code: '("hello" :: Number)',
                },
                {
                    locked: true,
                    type: "text",
                    text: "Why do we get an error? Well, we've told Wipple that `\"hello\"` is a number, but Wipple determined that it's actually text. Whenever there's a mismatch, Wipple issues an error.\n\nSometimes you might get an error that says \"could not determine the type of this code\". All that means is that you did something Wipple's algorithm couldn't figure out on its own, and you need to manually specify the type. You can do that with `::` as shown above!\n\nBut how does Wipple know that `n` is a `Number` in `n + 1`? Well, the full explanation is its own Advanced lesson, but for now, just know that Wipple keeps track of all the variables and how they are used throughout the program. When you pass a variable to a function or an operator like `+`, Wipple looks through the code of the function and infers what the input must be. If you use a single variable in two places that require different types, you'll get an error — a variable can't hold both a number and a piece of text at the same time!\n\nTypes are also used to enforce providing units for numbers, like in turtle graphics:",
                },
                {
                    type: "code",
                    code: "forward 50",
                    setup: "turtle",
                },
                {
                    locked: true,
                    type: "text",
                    text: "Click on the red bubble above to fix the error automatically!\n\nThere are many types built in to Wipple — here are a few:\n\n  -  `Number` and `Text` represent numbers and text\n  -  In `turtle`, `Pixels` represents a distance in `pixels`\n  -  In `music`, `Note-Length` represents a note length in `beats`\n  -  `List` represents a list of items\n  -  `Unit` represents \"nothing\"\n\nBy the way, you can hover your mouse over any piece of code and the Wipple Playground will show you its type. There are many other types, particularly `->` for functions, that are covered in the Advanced lessons, so don't worry if you see a type you aren't familiar with yet!\n\nIn the next lesson, we'll learn how to make our own types!",
                },
            ],
        },
        {
            name: "Making your own types",
            items: [
                {
                    locked: true,
                    text: "## Making your own types\n\nLet’s say we want to store some information about a sport. We can do that with multiple variables, like so:",
                    type: "text",
                },
                {
                    type: "code",
                    code: 'show-sport : name emoji players ->\n  show ("_ _ has _ players per team" name emoji players)\n\nbasketball-name : "Basketball"\nbasketball-emoji : "🏀"\nbasketball-players : 5\nshow-sport basketball-name basketball-emoji basketball-players\n\nsoccer-name : "Soccer"\nsoccer-emoji : "⚽️"\nsoccer-players : 11\nshow-sport soccer-name soccer-emoji soccer-players',
                },
                {
                    locked: true,
                    type: "text",
                    text: 'That code is pretty difficult to read, and every time we want to add a new sport, we need three more variables. What if we could "package up" all that information into a single variable?\n\nWell good news, because Wipple has something specifically designed for this problem — custom types!\n\nTo define a custom type, you use the `type` command and list out all the variables that go inside, along with their types:',
                },
                {
                    type: "code",
                    code: "Sport : type {\n  name :: Text\n  emoji :: Text\n  players :: Number\n}",
                },
                {
                    type: "text",
                    locked: true,
                    text: "If you're confused about what `::` means, make sure you read the previous lesson on Wipple's type system!\n\nAlright, now we can make a new sport by giving each of those variables a value:",
                },
                {
                    code: 'Sport : type {\n  name :: Text\n  emoji :: Text\n  players :: Number\n}\n\nbasketball : Sport {\n  name : "Basketball"\n  emoji : "🏀"\n  players : 5\n}\n\nsoccer : Sport {\n  name : "Soccer"\n  emoji : "⚽️"\n  players : 11\n}',
                    type: "code",
                },
                {
                    type: "text",
                    locked: true,
                    text: "Great! Notice the distinction between the double colon `::` and the single colon `:` — you always use the single colon when you're making a _value_ (like `\"Basketball\"` or `5`), and you only use the double colon when you're providing a _type_ (like `Text` or `Number`).\n\nSo now that we've packaged up our information into a single variable for each person, how do we get the information back out? If you put braces on the left-hand side of the `:`, you can rename each one of the fields:",
                },
                {
                    code: 'Sport : type {\n  name :: Text\n  emoji :: Text\n  players :: Number\n}\n\nbasketball : Sport {\n  name : "Basketball"\n  emoji : "🏀"\n  players : 5\n}\n\n{emoji : basketball-emoji} : basketball\nshow basketball-emoji',
                    type: "code",
                },
                {
                    text: "With all that in mind, we can rewrite the original example as follows:",
                    type: "text",
                    locked: true,
                },
                {
                    code: 'Sport : type {\n  name :: Text\n  emoji :: Text\n  players :: Number\n}\n\nshow-sport : {\n  name : name\n  emoji : emoji\n  players : players\n} -> show ("_ _ has _ players per team" name emoji players)\n\nbasketball : Sport {\n  name : "Basketball"\n  emoji : "🏀"\n  players : 5\n}\n\nsoccer : Sport {\n  name : "Soccer"\n  emoji : "⚽️"\n  players : 11\n}\n\nshow-sport basketball\nshow-sport soccer',
                    type: "code",
                },
                {
                    type: "text",
                    locked: true,
                    text: "Cool! Can you think of any other situations where a custom type is useful? Experiment in the space below!",
                },
                {
                    code: "",
                    type: "code",
                },
            ],
            id: "bS1-7daqoi6pK9haM3SH",
        },
        {
            name: "Patterns",
            id: "feq2Nk3BXBP75H_RpUiR",
            items: [
                {
                    type: "text",
                    locked: true,
                    text: "## Patterns\n\n`if` is great for making simple decisions about your input, but for more complicated cases it can get unwieldy:",
                },
                {
                    code: 'n : prompt "Enter a number"\nshow (if (n = 0) {"zero"} {if (n = 1) {"one"} {if (n = 2) {"two"} {"error"}}})',
                    type: "code",
                },
                {
                    text: "Luckily, Wipple has another function called `when` for this exact purpose! `when` accepts an input and a bunch of **patterns** to match. The first pattern that matches has its body executed. Let's see it in action:",
                    locked: true,
                    type: "text",
                },
                {
                    code: 'n : prompt "Enter a number"\n\nshow (when n {\n    0 -> "zero"\n    1 -> "one"\n    2 -> "two"\n    _ -> "error"\n})',
                    type: "code",
                },
                {
                    text: "In fact, `if` is actually `when` under the hood, matching on `True` and `False`!\n\nLet's go through this code more thoroughly. First, we provide the input to `when`. Then, we list all the cases we want to handle. The last case, the underscore `_`, serves as a \"catch-all\" pattern that handles any number that's not 0, 1 or 2. The right-hand side of a function is only run if the input matches the pattern on the left-hand side. Only the first function to match is called; after that, `when` exits.\n\n`when` also works for text!",
                    locked: true,
                    type: "text",
                },
                {
                    type: "code",
                    code: 'key : prompt "Enter the secret key"\nshow (when key {\n    "Wipple" -> "Access granted!"\n    _ -> "Access denied!"\n})',
                },
                {
                    locked: true,
                    type: "text",
                    text: "A pattern can also be a variable:",
                },
                {
                    code: 'when "hi" {\n    x -> show ("x is _" x)\n    y -> show ("y is _" y)\n}',
                    type: "code",
                },
                {
                    locked: true,
                    type: "text",
                    text: "Notice that only one of the functions (the one for `x`) is called, and the input is assigned to the `x` variable and displayed.\n\nAlso, it's important to note that `x` is only available on the right-hand side of the function — you can't use it after the function finishes! This is different from other languages like Python, where you can assign a variable in each branch of an `if` statement and use it afterward. But this isn't usually a problem in Wipple, because `when` is just a function that returns a value, which you can assign to a variable right away!",
                },
                {
                    code: 'name : "Wipple"\n\ngreeting : when name {\n    "Wipple" -> "My favorite language!"\n    _ -> "Hello, _!" name\n}\n\nshow greeting',
                    type: "code",
                },
                {
                    locked: true,
                    text: "You can also use `or` to match one of two or more patterns in the same function:",
                    type: "text",
                },
                {
                    type: "code",
                    code: 'name : "JavaScript"\n\ngreeting : when name {\n    "Wipple" -> "My favorite language!"\n    "Python" or "JavaScript" -> "_ is cool too!" name\n    _ -> "Hello, _!" name\n}\n\nshow greeting',
                },
                {
                    text: "If you want to run multiple lines of code inside a `when`, you can use `do`:",
                    type: "text",
                    locked: true,
                },
                {
                    code: 'when (1 + 1) {\n    2 -> do {\n        show "Woohoo!"\n        show "Math works!"\n    }\n    _ -> show "Uh oh..."\n}',
                    type: "code",
                },
                {
                    type: "text",
                    text: "Thanks to Wipple's type system, using `when` instead of `if` is actually a better choice in most cases, because `when` ensures that you handle all possible cases. How does that work? Let's take a look in the next lesson!",
                    locked: true,
                },
            ],
        },
        {
            items: [
                {
                    locked: true,
                    text: "## Making your own patterns\n\nIn the last lesson, we learned how to use `when` to match an input to one of several patterns. But `when` doesn't just work with `Numbers` and `Text` — we can define our own patterns!\n\nLet's say we want to build a program that creates a report card given a grade. We can represent the grades using text...",
                    type: "text",
                },
                {
                    type: "code",
                    code: 'grade : "A"\n\nreport-card : when grade {\n    "A" -> "Top of the class"\n    "B" -> "Good work"\n    "C" -> "Getting there"\n    "D" or "F" -> "Need to study"\n    _ -> "Invalid grade"\n}\n\nshow report-card',
                },
                {
                    locked: true,
                    text: 'Notice that we need to have the `_` catch-all pattern at the end to handle the case where `grade` isn\'t `"A"` through `"F"`. But can we do better?\n\nIn fact, we can! We can declare our own patterns and put them in a `type`, and then Wipple can _guarantee_ that all grades are valid grades.',
                    type: "text",
                },
                {
                    code: "Grade : type {\n  A\n  B\n  C\n  D\n  F\n}",
                    type: "code",
                },
                {
                    locked: true,
                    type: "text",
                    text: "Notice that instead of putting variables inside the `type`, we're putting _patterns_. Custom patterns begin with an uppercase letter (whereas variables are lowercase).\n\nAnd now we can build a new `report-card` function!",
                },
                {
                    type: "code",
                    code: 'Grade : type {\n  A\n  B\n  C\n  D\n  F\n}\n\ngrade : A\n\nreport-card : when grade {\n    A -> "Top of the class"\n    B -> "Good work"\n    C -> "Getting there"\n    D or F -> "Need to study"\n}\n\nshow report-card',
                },
                {
                    text: "Look closely — we don't have the `_` catch-all pattern anymore. We don't need to have it, because if we try to make an invalid grade, we get an error!",
                    type: "text",
                    locked: true,
                },
                {
                    type: "code",
                    code: "Grade : type {\n  A\n  B\n  C\n  D\n  F\n}\n\ngrade : E",
                },
                {
                    text: "And now that we've told Wipple all the possible `Grade`s, Wipple tells us if we forgot to handle one of them!",
                    locked: true,
                    type: "text",
                },
                {
                    type: "code",
                    code: 'Grade : type {\n  A\n  B\n  C\n  D\n  F\n}\n\ngrade : A\n\nreport-card : when grade {\n    A -> "Top of the class"\n    B -> "Good work"\n    C -> "Getting there"\n}\n\nshow report-card',
                },
                {
                    locked: true,
                    text: "Wipple’s patterns are powerful and expressive, and there’s a lot more to them than this. If you’re curious about more advanced patterns, check out the Advanced lessons!",
                    type: "text",
                },
            ],
            id: "jmur9w45wEKVFjoTK6iP",
            name: "Making your own patterns",
        },
        {
            name: "Graphing functions",
            id: "XzcpK-7sGaoIo7iv2rv7",
            items: [
                {
                    type: "text",
                    text: "## Graphing functions\n\nLet's end the Intermediate lessons with another practical use for Wipple — plotting mathematical functions on a graph! If you create a Math program, you can use `plot` to plot a function:",
                    locked: true,
                },
                {
                    type: "code",
                    setup: "math",
                    code: "plot (x -> x ^ 2)",
                },
                {
                    text: "The graph looks like that by default. But we can change the bounds as well as the color!",
                    locked: true,
                    type: "text",
                },
                {
                    code: 'min-y 0\nmax-y 100\ncolor [Color "#3b82f6"]\nplot (x -> x ^ 2)',
                    type: "code",
                    setup: "math",
                },
                {
                    locked: true,
                    type: "text",
                    text: "You can also graph multiple functions at the same time, each with its own color:",
                },
                {
                    code: 'color [Color "#ef4444"]\nplot (x -> x)\n\ncolor [Color "#22c55e"]\nplot (x -> x ^ 2)\n\ncolor [Color "#3b82f6"]\nplot (x -> x ^ 3)',
                    type: "code",
                    setup: "math",
                },
                {
                    locked: true,
                    text: "This next example makes a zig-zag by using `%`, which returns the remainder of dividing two numbers. For example, `10 % 4` is `2` and `2 % 2` is `0`.",
                    type: "text",
                },
                {
                    code: "plot (x -> x % 2)",
                    setup: "math",
                    type: "code",
                },
                {
                    text: "Go ahead and graph your own functions in the space below!",
                    type: "text",
                    locked: true,
                },
                {
                    setup: "math",
                    code: "",
                    type: "code",
                },
                {
                    type: "text",
                    text: "### Great work!\n\nCongratulations, you've made it to the end of the Intermediate lessons! 😎 Feel free to explore the Wipple Playground and write your own programs. If you want to learn more, try moving on to the Advanced lessons.",
                    locked: true,
                },
            ],
        },
    ],
};
