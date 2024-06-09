import { Lesson } from "./index";

export const learnPhysicsLesson: Lesson = {
    id: "learn-physics",
    name: "Learn Physics",
    description: "Explore physics by building your own demonstrations.",
    pages: [
        {
            id: "vo37ftM5yR69ZGzDIul9",
            name: "Units",
            items: [
                {
                    text: "## Units\n\nWelcome! In this course, you'll learn mechanics by building your own physics demonstrations in Wipple. But before we dive into formulas, let's look at how to use units to make sure our calculations make sense.\n\nThere are four main quantities we're going to deal with in our first lesson: **time** (seconds), **distance** (meters), **velocity** (meters per second), and **acceleration** (meters per second, per second). In every calculation we do with these quantities, we'll need to specify the units. Not only does it help us make sense of the relationships between the numbers, but it also helps Wipple identify any incompatible units and flag bugs in our code!\n\n## Standing still\n\nYou can use the `position` command to set the $x$ and $y$ position of an object (in this case, the green block). `constant` means that the position doesn't change.",
                    type: "text",
                    locked: true,
                },
                {
                    settings: {
                        demonstration: "One Block",
                    },
                    setup: "physics",
                    type: "code",
                    code: 'x : constant (0 meters)\ny : constant (0 meters)\nposition [Object "Block 1"] x y',
                },
            ],
        },
        {
            id: "4RGRi057AyJwndTTFBHd",
            items: [
                {
                    text: "## Constant motion\n\nNext, let's look at an object moving in a straight line. That means the object has a **velocity** (speed). But how do we express that in code?\n\nLet's work out this problem with units. We know velocity has units of **meters per second**, and position has units of **meters**. So to cancel out the denominator, we need to multiply velocity by something that has units of **seconds**. Well, we already have something in seconds — the current time $t$!\n\nLet's replace `constant` with a function accepting the current time:",
                    locked: true,
                    type: "text",
                },
                {
                    code: 'x : t -> 1 (meters / seconds) * t\ny : constant (0 meters)\nposition [Object "Block 1"] x y',
                    type: "code",
                    setup: "physics",
                    settings: {
                        demonstration: "One Block",
                    },
                },
                {
                    locked: true,
                    type: "text",
                    text: "If you don't see the box above, click **Run again** to restart the demonstration.\n\nRight now, the box starts in the middle of the screen. That's because the demonstration starts at time 0, and multiplying our velocity by 0 produces 0. How do you think we change the starting position? Try changing the code below!",
                },
                {
                    setup: "physics",
                    code: 'x : t -> 1 (meters / seconds) * t\ny : constant (0 meters)\nposition [Object "Block 1"] x y',
                    type: "code",
                    settings: {
                        demonstration: "One Block",
                    },
                },
                {
                    type: "text",
                    text: "The solution is to **add** an initial distance. Let's make it start a bit to the right:",
                    locked: true,
                },
                {
                    type: "code",
                    setup: "physics",
                    settings: {
                        demonstration: "One Block",
                    },
                    code: 'x : t -> 1 meters + 1 (meters / seconds) * t\ny : constant (0 meters)\nposition [Object "Block 1"] x y',
                },
                {
                    locked: true,
                    type: "text",
                    text: "Awesome!\n\nLet’s make one more change to our code — we can give names to our numbers:",
                },
                {
                    code: 'x0 : 1 meters\nv : 1 (meters / seconds)\n\nx : t -> x0 + v * t\ny : constant (0 meters)\nposition [Object "Block 1"] x y',
                    type: "code",
                    settings: {
                        demonstration: "One Block",
                    },
                    setup: "physics",
                },
                {
                    locked: true,
                    type: "text",
                    text: "And with that, we have our first **kinematic equation** to model an object moving at a constant speed:\n\n$$$\nx(t) = x_0 + vt\n$$$\n\nYou can practice building your own demonstrations in the space below!",
                },
                {
                    settings: {
                        demonstration: "One Block",
                    },
                    type: "code",
                    code: "",
                    setup: "physics",
                },
            ],
            name: "Constant motion",
        },
        {
            items: [
                {
                    locked: true,
                    type: "text",
                    text: "## Force, mass, and acceleration\n\nLet's say we want to slide a block across the floor. When you push a block, you're applying a **force**. You already know that if you push two blocks of different weights, the lighter block is going to speed up more quickly, but how do we measure that speed? Let's experiment!\n\nWe'll start with two blocks on the screen, one green and one orange. We can get the **mass** of each block using `mass-of`:",
                },
                {
                    type: "code",
                    setup: "physics",
                    code: 'show ("Green block: _" (mass-of [Object "Block 1"]))\nshow ("Orange block: _" (mass-of [Object "Block 2"]))',
                    settings: {
                        demonstration: "One Block",
                    },
                },
                {
                    type: "text",
                    locked: true,
                    text: "Now, let's use `force` (measured in `newtons`) to apply the same vertical force to both blocks:",
                },
                {
                    setup: "physics",
                    code: 'fx : constant (0 newtons)\nfy : constant (1 newtons)\n\nforce [Object "Block 1"] fx fy\nforce [Object "Block 2"] fx fy\n\nobserve (1 seconds) {\n  show ("The green block is moving at _" (y-velocity-of [Object "Block 1"]))\n  show ("The orange block is moving at _" (y-velocity-of [Object "Block 2"]))\n}',
                    settings: {
                        demonstration: "Two Blocks",
                    },
                    type: "code",
                },
                {
                    text: "Using `observe`, we can measure the exact velocity of each block after pushing for one second, and sure enough, the green block is moving more quickly! If you don't see the blocks, they went off the screen — try clicking **Run Again**.\n\nDo you notice anything else about the simulation? The blocks keep **speeding up** so long as force is being applied. Let's try to measure this!",
                    locked: true,
                    type: "text",
                },
                {
                    settings: {
                        demonstration: "One Block",
                    },
                    setup: "physics",
                    type: "code",
                    code: 'fx : constant (1 newtons)\nfy : constant (0 newtons)\n\nforce [Object "Block 1"] fx fy\n\n(0 to 4 by 1) . transform seconds . each (t -> do {\n  observe t {\n    show ("At _, the block is moving at _" t (x-velocity-of [Object "Block 1"]))\n  }\n})',
                },
                {
                    type: "text",
                    text: "This speeding up is called **acceleration**, which we measured to be a change of 1 meter per \nsecond every second. As the force gets larger, the acceleration increases, and as the block gets heavier, the acceleration decreases. Let's write a formula for this!\n\n$$$\na = \\frac{F}{m}\n$$$\n\nIf we multiply both sides by $m$, we get **Newton's Second Law:**\n\n$$$\nF = m \\cdot a\n$$$\n\nEarlier, we said force is measured in **Newtons** — that's why! But let's plug in units in place of $m$ and $a$ in the equation, and see what we get:\n\n$$$\nF = \\text{kg} \\cdot \\text{m/s/s}\n$$$\n\nThat means one Newton is **equivalent** to one kilogram per unit acceleration! And sure enough, the simulation recognizes this:",
                    locked: true,
                },
                {
                    code: 'fx : constant (1 (kilograms * (meters / seconds / seconds)))\nfy : constant (0 (kilograms * (meters / seconds / seconds)))\n\nforce [Object "Block 1"] fx fy',
                    settings: {
                        demonstration: "One Block",
                    },
                    setup: "physics",
                    type: "code",
                },
                {
                    locked: true,
                    type: "text",
                    text: "Of course, writing `newtons` is usually more convenient, but it helps to see Newton's Second Law right in the units.",
                },
            ],
            name: "Force, mass, and acceleration",
            id: "LnUMuu_VZWvxf3WkUc4N",
        },
        {
            name: "Kinematics",
            id: "4qnncjXg43IdMDZO63v-",
            items: [
                {
                    type: "text",
                    text: "## Kinematics\n\nNow that we know Newton's Second Law, we can use it to solve for an object's acceleration and make our first kinematic equation more useful!\n\nWe know $a = \\frac{F}{m}$, so we can write a function to calculate the blocks' acceleration after a given time:",
                    locked: true,
                },
                {
                    type: "code",
                    code: 't : 1 seconds\n\nfx : constant (0 newtons)\nfy : constant (1 newtons)\n\nforce [Object "Block 1"] fx fy\nforce [Object "Block 2"] fx fy\n\ny-acceleration-of : object -> (fy t) / (mass-of object)\n\nobserve t {\n  show ("The green block is accelerating at _" (y-acceleration-of [Object "Block 1"]))\n  show ("The orange block is accelerating at _" (y-acceleration-of [Object "Block 2"]))\n}',
                    settings: {
                        demonstration: "Two Blocks",
                    },
                    setup: "physics",
                },
                {
                    locked: true,
                    type: "text",
                    text: "And now we can write our measurements down in code. We'll start with just the green block:",
                },
                {
                    setup: "physics",
                    settings: {
                        demonstration: "One Block",
                    },
                    type: "code",
                    code: "x0 : 0 meters\nv : 0 (meters / seconds)\na : 1 (meters / seconds / seconds)",
                },
                {
                    text: "Now we need to write a position function that accepts $t$ and produces the distance the object has traveled up to that point in time. We know we can multiply $v$ by $t$ to cancel out the time unit, so what do you think we need to do for acceleration?\n\nThe answer is that we need to multiply by time **twice**! Multiplying by time once gets us the **velocity** at that time, and then we can multiply velocity by time to get position. Let's try it!",
                    locked: true,
                    type: "text",
                },
                {
                    setup: "physics",
                    settings: {
                        demonstration: "One Block",
                    },
                    code: 'x0 : 0 meters\nv : 0 (meters / seconds)\na : 1 (meters / seconds / seconds)\n\nx : t -> x0 + (v * t) + (a * t * t)\ny : constant (0 meters)\n\nposition [Object "Block 1"] x y',
                    type: "code",
                },
                {
                    text: "Hmm... do you notice the block is speeding up too quickly? Let's add in some measurements and see by how much compared to using forces.",
                    type: "text",
                    locked: true,
                },
                {
                    code: 'x0 : 0 meters\nv : 0 (meters / seconds)\na : 1 (meters / seconds / seconds)\n\nx : t -> x0 + (v * t) + (a * t * t)\ny : constant (0 meters)\n\nposition [Object "Block 1"] x y\n\nobserve (1 seconds) {\n  show ("The block is moving at _" (x-velocity-of [Object "Block 1"]))\n}',
                    type: "code",
                    settings: {
                        demonstration: "One Block",
                    },
                    setup: "physics",
                },
                {
                    locked: true,
                    type: "text",
                    text: "It's twice the velocity! That means to make our kinematic equation accurate, we need to multiply our acceleration term by $\\frac{1}{2}$. (This has to do with the squaring of $t$.)",
                },
                {
                    settings: {
                        demonstration: "One Block",
                    },
                    type: "code",
                    setup: "physics",
                    code: 'x0 : 0 meters\nv : 0 (meters / seconds)\na : 1 (meters / seconds / seconds)\n\nx : t -> x0 + (v * t) + (1 / 2 * a * t * t)\ny : constant (0 meters)\n\nposition [Object "Block 1"] x y\n\nobserve (1 seconds) {\n  show ("The block is moving at _" (x-velocity-of [Object "Block 1"]))\n}',
                },
                {
                    type: "text",
                    locked: true,
                    text: "Perfect! And with that, we have the **first kinematic equation**:\n\n$$$\nx = x_0 + vt + \\frac{1}{2}at^2\n$$$",
                },
            ],
        },
    ],
};
