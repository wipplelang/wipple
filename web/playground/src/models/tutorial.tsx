import { MaterialSymbol } from "react-material-symbols";

interface TutorialStepBase {
    body: JSX.Element;
    backButton: "back" | "hidden";
    continueButton: "continue" | "hidden" | "end";
}

interface TutorialStepBuilder extends TutorialStepBase {
    itemId?: string;
    clickItemToContinue?: boolean;
}

export interface TutorialStep extends TutorialStepBuilder {
    onBack: () => void;
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
            onBack: () => onChangeStep(steps[Math.max(0, index - 1)]),
            onClickItem: builder.clickItemToContinue ? onContinue : undefined,
            onContinue,
        };
    });

    return steps[0];
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
        backButton: "hidden",
        continueButton: "continue",
    },
    {
        body: (
            <p>
                Let's create a new playground. Click on the <strong>New Playground</strong> button.
            </p>
        ),
        backButton: "back",
        continueButton: "hidden",
        itemId: "newPlayground",
        clickItemToContinue: true,
    },
];
