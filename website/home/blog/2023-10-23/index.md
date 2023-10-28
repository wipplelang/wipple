---
layout: blog
title: Teaching Wipple in elementary school
date: 2023-10-23
image: /blog/2023-10-23/images/tes.jpg
---

Last week, I visited Tyngsborough Elementary School in Tyngsborough, Massachusetts to teach Wipple to the 4th and 5th graders in Science and Technology class. Students spent about 30 minutes creating a Turtle drawing using the Wipple Playground. For many students, Wipple was their first text-based programming language, so learning Wipple was also an opportunity to practice typing. As I walked around the classroom, I was amazed by everyone's creativity!

![Teaching Wipple at Tyngsborough Elementary School](/blog/2023-10-23/images/tes.jpg)

On my first day at TES, I showed the students how to use `repeat` to run a block of code multiple times. I instructed them to copy their code to the clipboard, type `repeat (4 times) {}`, and paste their code between the braces. I noticed many students found this challenging — all the keyboard shortcuts were overwhelming them and taking away their attention from the code itself. So this week, I made three changes to make Wipple more mouse-friendly: an Edit menu, snippets, and a new Insert button!

### Edit menu

Previously, the code editor in the Wipple Playground operated entirely on keyboard shortcuts. Now, there's a new Edit menu in the top left of the screen to activate common commands like Copy, Paste, and Select All with your mouse. Keyboard shortcuts are an important part of learning to type, but I want first-timers to be able to write and manipulate short programs quickly without getting overwhelmed. Learning to code is already a challenging task!

![Screenshot of the new Edit menu in the Wipple Playground](/blog/2023-10-23/images/edit-menu.png)

### Snippets

Wipple now supports defining "snippets" that expand to new code or wrap existing code. Here are a few examples:

```wipple
snippet "Move forward" : forward (50 pixels)

snippet "Show" : show 'code

snippet "Repeat" : repeat (5 times) {
  'code
}
```

These snippets are parsed by the compiler and are available in the analyzed program so IDEs can suggest them. You write snippets alongside your other code, all in the same file, so there's no setup required. You can also insert a placeholder (`'code`) to indicate that the snippet wraps the user's selection.

### Insert button

The + button in the top right of the editor now lives right next to your cursor, and suggests snippets based on your selection. Check it out!

![Demonstration of the insert button in the Wipple Playground](/blog/2023-10-23/images/insert-button-demo.gif)

Once I made these changes, students were able to get up to speed much more quickly and spend more time being creative. I think the best way to learn to code is by applying what you're already passionate about, and I look forward to bringing Wipple to more students in the near future. I can't wait to see what they'll create!
