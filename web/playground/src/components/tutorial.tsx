import { MaterialSymbol } from "react-material-symbols";
import { useStore } from "../store";
import {
    ClientRectObject,
    FloatingPortal,
    autoUpdate,
    size,
    useFloating,
} from "@floating-ui/react";
import { produce } from "immer";
import { useEffect, useState } from "react";
import { useWindowSize } from "usehooks-ts";

export const TutorialSection = () => {
    const [store, setStore] = useStore();

    return store.activeTutorialStep ? (
        <div className="flex flex-row gap-3 w-full h-full p-4 rounded-lg bg-gray-50 dark:bg-gray-950">
            {store.activeTutorialStep.image ? (
                <img
                    src={store.activeTutorialStep.image}
                    className="h-[6lh] aspect-auto rounded-lg border-2 border-gray-100 dark:border-gray-900"
                />
            ) : null}

            <div
                className={`flex flex-col gap-3 justify-between w-full ${
                    store.activeTutorialStep.image ? "h-[6lh]" : ""
                }`}
            >
                <div className="flex flex-row items-start justify-between gap-2">
                    <div className="mt-0.5 ml-0.5 flex flex-row gap-4">
                        {store.activeTutorialStep.body}
                    </div>

                    <TutorialOverlayButton
                        onClick={() => {
                            if (confirm("End the tutorial?")) {
                                setStore(
                                    produce((store) => {
                                        store.activeTutorialStep = undefined;
                                    }),
                                );
                            }
                        }}
                    >
                        <MaterialSymbol icon="close" className="text-2xl -mt-1" />
                    </TutorialOverlayButton>
                </div>

                <div className="flex flex-row justify-end">
                    {store.activeTutorialStep.continueButton !== "hidden" ? (
                        <TutorialOverlayButton onClick={store.activeTutorialStep.onContinue}>
                            <div className="flex flex-row items-center gap-1 px-1">
                                {store.activeTutorialStep.continueButton === "end"
                                    ? "End Tutorial"
                                    : "Continue"}
                                <MaterialSymbol icon="arrow_forward_ios" className="text-xl w-4" />
                            </div>
                        </TutorialOverlayButton>
                    ) : null}
                </div>
            </div>
        </div>
    ) : null;
};

const TutorialOverlayButton = (props: React.PropsWithChildren<{ onClick: () => void }>) => (
    <button
        onClick={props.onClick}
        className="text-gray-800 dark:text-gray-400 text-opacity-65 hover:text-opacity-100 transition-colors rounded-md"
    >
        {props.children}
    </button>
);

const highlightPadding = 8;

export const TutorialItem = (
    props: React.PropsWithChildren<{
        id?: string;
        className?: string;
    }>,
) => {
    const [store, _setStore] = useStore();

    const isActive = props.id != null && store.activeTutorialStep?.itemId === props.id;

    const windowSize = useWindowSize();
    const [rect, setRect] = useState<DOMRect | ClientRectObject>();
    const [padding, setPadding] = useState(0);

    useEffect(() => {
        if (!isActive) {
            return;
        }

        let trackedPadding = highlightPadding + 20;
        setPadding(trackedPadding);
        const interval = setInterval(() => {
            if (trackedPadding === highlightPadding) {
                clearInterval(interval);
            }

            trackedPadding -= 1;
            setPadding(trackedPadding);
        }, 10);
    }, [isActive, props.id]);

    const { refs } = useFloating({
        open: store.activeTutorialStep?.itemId === props.id,
        transform: false,
        whileElementsMounted: autoUpdate,

        middleware: [
            size({
                apply: ({ elements }) => {
                    const rect = elements.reference.getBoundingClientRect();
                    setRect(rect);
                },
            }),
        ],
    });

    return (
        <>
            <div
                ref={refs.setReference}
                className={props.className}
                onClick={store.activeTutorialStep?.onClickItem}
            >
                {props.children}
            </div>

            {isActive ? (
                // Dim background except over the highlighted element
                <FloatingPortal>
                    <div
                        ref={refs.setFloating}
                        className="fixed inset-0 text-black text-opacity-25 pointer-events-none"
                    >
                        <svg width={windowSize.width} height={windowSize.height}>
                            <rect
                                x={(rect?.x ?? 0) - padding}
                                y={(rect?.y ?? 0) - padding}
                                width={(rect?.width ?? 0) + 2 * padding}
                                height={(rect?.height ?? 0) + 2 * padding}
                                rx="12"
                                ry="12"
                                className="fill-transparent stroke-2 stroke-blue-500"
                            />
                        </svg>
                    </div>
                </FloatingPortal>
            ) : null}
        </>
    );
};
