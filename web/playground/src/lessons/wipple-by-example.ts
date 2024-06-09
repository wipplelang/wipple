import { Lesson } from "./index";

export const wippleByExampleLesson: Lesson = {
    id: "wipple-by-example",
    name: "Wipple by Example",
    description: "Learn how to use all of Wipple's features.",
    pages: [
        {
            items: [
                {
                    locked: true,
                    text: "## Variables\n\nThe `:` operator is used to assign a value to a variable so that it may be referred to later in the program:",
                    type: "text",
                },
                {
                    code: "sum : 1 + 2\nshow sum",
                    type: "code",
                },
                {
                    text: "You can declare two variables with the same name, and the newer variable will “shadow” the older variable, meaning the older variable cannot be accessed anymore within the current scope. The older variable does remain accessible inside blocks and functions that captured it before it was shadowed:",
                    locked: true,
                    type: "text",
                },
                {
                    code: 'greeting : "Hello"\nshow-greeting : {show greeting}\n\ngreeting : "Howdy"\nshow greeting\n\ndo show-greeting',
                    type: "code",
                },
            ],
            id: "Tz5bnIlXjPE_8pugoMfI",
            name: "Variables",
        },
        {
            id: "8YDeRYs7yM2LnLm3wQyW",
            name: "Functions",
            items: [
                {
                    locked: true,
                    text: "## Functions\n\nThe `->` operator is used to create functions. The inputs go on the left, and the output goes on the right:",
                    type: "text",
                },
                {
                    type: "code",
                    code: "add : a b -> a + b\nshow (add 1 2)",
                },
                {
                    locked: true,
                    text: "The `->` operator is also commonly used inside `when` to specify a pattern to match and the code to run if the pattern matches:",
                    type: "text",
                },
                {
                    type: "code",
                    code: 'Grade : type {\n  A\n  B\n  C\n  D\n  F\n}\n\nreport-card : grade -> when grade {\n    A -> "top of the class"\n    B -> "good job"\n    C -> "need to study"\n    D or F -> "didn\'t pass"\n}\n\nshow (report-card A)',
                },
            ],
        },
    ],
};
