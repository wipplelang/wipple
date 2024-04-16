import { useEffect, useState } from "react";
import { MaterialSymbol } from "react-material-symbols";
import { useStore } from "../store";
import { PlaygroundPageItem } from "./playground";

interface TutorialStepBase {
    body: JSX.Element;
    continueButton: "continue" | "hidden" | "end";
}

interface TutorialStepBuilder extends TutorialStepBase {
    actionId?: string;
    itemId?: string;
    clickItemToContinue?: boolean;
}

export interface TutorialStep extends TutorialStepBuilder {
    onClickItem?: () => void;
    onContinue: () => void;
}

export const startTutorial = (onChangeStep: (step: TutorialStep | undefined) => void) => {
    const steps: TutorialStep[] = tutorialSteps.map((builder, index) => {
        const onContinue = () => {
            if (builder.continueButton === "end") {
                onChangeStep(undefined);
            } else {
                onChangeStep(steps[Math.min(steps.length - 1, index + 1)]);
            }
        };

        return {
            ...builder,
            index,
            onClickItem: builder.clickItemToContinue ? onContinue : undefined,
            onContinue,
        };
    });

    return steps[0];
};

export const useOnTutorialAction = (
    actionId: string,
    action: () => boolean,
    deps: React.DependencyList,
) => {
    const [store, _setStore] = useStore();

    const [ran, setRan] = useState(false);
    useEffect(() => {
        if (!ran && store.activeTutorialStep?.actionId === actionId) {
            setRan(action());
        }
    }, [actionId, ran, store.activeTutorialStep?.actionId, ...deps]);
};

const tutorialSteps: TutorialStepBuilder[] = [
    {
        body: (
            <p>
                Welcome to the Wipple Playground! Click <strong>Continue</strong> to start the
                tutorial, or{" "}
                <MaterialSymbol icon="close" weight={600} className="text-xl align-middle" /> to
                close the tutorial at any time.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Let's create a new playground. Click on the <strong>New Playground</strong> button.
            </p>
        ),
        continueButton: "hidden",
        itemId: "newPlayground",
        clickItemToContinue: true,
    },
    {
        body: <p>This is the playground editor.</p>,
        actionId: "newPlayground",
        continueButton: "continue",
    },
    {
        body: <p>Playgrounds can have multiple pages, which are listed on the left.</p>,
        continueButton: "continue",
        itemId: "playgroundPageList",
    },
    {
        body: <p>Your code is on the right. </p>,
        continueButton: "continue",
        itemId: "playgroundCodeEditor",
    },
    {
        body: (
            <p>
                Let's make the turtle draw a square! Click <strong>Commands</strong>.
            </p>
        ),
        continueButton: "hidden",
        clickItemToContinue: true,
        itemId: "commandsButton",
    },
    {
        body: (
            <p>
                Now click on{" "}
                <strong>
                    <code>forward</code>
                </strong>{" "}
                and drag it into the code box. When you're done, click <strong>Continue</strong>.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Now the turtle needs to turn left. Go ahead and drag{" "}
                <strong>
                    <code>left</code>
                </strong>{" "}
                into the code editor from the <strong>Commands</strong> menu.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Great, we have one side of the square! Now we need to draw the remaining sides.
                Click <strong>Continue</strong> when you're ready.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                To draw the remaining sides, we're going to use{" "}
                <strong>
                    <code>repeat</code>
                </strong>
                . To use <code>repeat</code>, we need to select all our code. Click the{" "}
                <strong>Edit</strong> button.
            </p>
        ),
        continueButton: "hidden",
        clickItemToContinue: true,
        itemId: "editButton",
    },
    {
        body: (
            <p>
                Click <strong>Select All</strong>.
            </p>
        ),
        continueButton: "hidden",
        clickItemToContinue: true,
        itemId: "selectAll",
    },
    {
        body: (
            <p>
                Now that our code is selected, we can drag{" "}
                <strong>
                    <code>repeat</code>
                </strong>{" "}
                over it. Go ahead and drag{" "}
                <strong>
                    <code>repeat</code>
                </strong>{" "}
                onto your selection from the <strong>Commands</strong> menu.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Finally, click the dropdown and change it to{" "}
                <strong>
                    <code>4 times</code>.
                </strong>
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: <p>Woo-hoo, you should see a square!</p>,
        continueButton: "continue",
    },
    {
        body: (
            <p>
                If you ever want to know what a command does, you can use <strong>Look Up</strong>.
                Try clicking on the magnifying glass button.
            </p>
        ),
        continueButton: "hidden",
        clickItemToContinue: true,
        itemId: "lookUpButton",
    },
    {
        body: (
            <p>
                Move your mouse over <code>repeat</code>, <code>forward</code>, and{" "}
                <code>left</code> to learn more about them. When you're done, click{" "}
                <strong>Continue</strong>.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Click <strong>Done</strong> to close Look Up.
            </p>
        ),
        continueButton: "hidden",
        clickItemToContinue: true,
        itemId: "lookUpButton",
    },
    {
        body: (
            <p>
                Try changing the size of the square using the dropdown next to{" "}
                <strong>
                    <code>forward</code>
                </strong>
                . If you need more space, drag the bottom-right corner of the canvas to resize it.
                When you're done, click <strong>Continue</strong>.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Time for a challenge — can you make a hexagon? You'll need all three dropdowns. Once
                you've got it, click <strong>Continue</strong> to see the solution.
            </p>
        ),
        continueButton: "continue",
    },
    {
        body: (
            <p>
                To make a hexagon, the turtle needs to turn <strong>6 times</strong>.
            </p>
        ),
        actionId: "hexagonCode",
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Now it's your turn — feel free to draw any shape you want! Don't forget, you can add
                more lines of code from the <strong>Commands</strong> menu.
            </p>
        ),
        continueButton: "end",
    },
];

export const newPlaygroundTutorialItem: PlaygroundPageItem = {
    type: "code",
    setup: "turtle",
    code: "",
};

export const hexagonTutorialItem: PlaygroundPageItem = {
    type: "code",
    setup: "turtle",
    code: `repeat ([Dropdown (1 , 2 , 3 , 4 , 5 , 10 , 20 , 50 , 100) 6] times) {
  forward ([Dropdown (10 , 20 , 30 , 40 , 50 , 100 , 200 , 500) 50] pixels)
  left ([Dropdown (10 , 20 , 30 , 45 , 60 , 90 , 180 , 270 , 360) 60] degrees)
}`,
};
