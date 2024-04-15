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

    return steps[4]; // FIXME: Reset to 0
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
            action();
            setRan(true);
        }
    }, [actionId, ran, ...deps]);
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
];

export const newPlaygroundTutorialItem: PlaygroundPageItem = {
    type: "code",
    setup: "turtle",
    code: "",
};
