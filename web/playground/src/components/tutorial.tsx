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
import { useState } from "react";
import { useWindowSize } from "usehooks-ts";

export const TutorialOverlay = (props: React.PropsWithChildren<{}>) => {
    const [store, setStore] = useStore();

    return (
        <div className="relative">
            <div>{props.children}</div>

            {store.activeTutorialStep ? (
                <>
                    <FloatingPortal>
                        <div className="fixed left-0 right-0 bottom-8 mx-auto w-[480px] bg-white p-4 rounded-xl border-2 border-gray-100 dark:border-gray-800 overflow-clip shadow-lg z-[9999]">
                            <div className="flex flex-col gap-3 w-full h-full">
                                <div className="flex flex-row items-start justify-between gap-2">
                                    <div className="mt-0.5 ml-0.5">
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
                                        <TutorialOverlayButton
                                            onClick={store.activeTutorialStep.onContinue}
                                        >
                                            <div className="flex flex-row items-center gap-1 px-1">
                                                {store.activeTutorialStep.continueButton === "end"
                                                    ? "End Tutorial"
                                                    : "Continue"}
                                                <MaterialSymbol
                                                    icon="arrow_forward_ios"
                                                    className="text-xl w-4"
                                                />
                                            </div>
                                        </TutorialOverlayButton>
                                    ) : null}
                                </div>
                            </div>
                        </div>
                    </FloatingPortal>
                </>
            ) : null}
        </div>
    );
};

const TutorialOverlayButton = (props: React.PropsWithChildren<{ onClick: () => void }>) => (
    <button
        onClick={props.onClick}
        className="text-gray-800 dark:text-gray-400 text-opacity-65 hover:text-opacity-100 transition-colors rounded-md"
    >
        {props.children}
    </button>
);

const highlightPadding = 12;

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
                            <defs>
                                <mask id="mask">
                                    <rect width="100%" height="100%" fill="white" />
                                    <rect
                                        x={(rect?.x ?? 0) - highlightPadding}
                                        y={(rect?.y ?? 0) - highlightPadding}
                                        width={(rect?.width ?? 0) + 2 * highlightPadding}
                                        height={(rect?.height ?? 0) + 2 * highlightPadding}
                                        rx="12"
                                        ry="12"
                                        fill="black"
                                    />
                                </mask>
                            </defs>

                            <rect
                                width={windowSize.width}
                                height={windowSize.height}
                                fill="currentColor"
                                mask="url(#mask)"
                            />
                        </svg>
                    </div>
                </FloatingPortal>
            ) : null}
        </>
    );
};
