import { MaterialSymbol } from "react-material-symbols";
import { useStore } from "../store";
import { FloatingPortal, offset, size, useFloating } from "@floating-ui/react";
import { produce } from "immer";

export const TutorialOverlay = (props: React.PropsWithChildren<{}>) => {
    const [store, setStore] = useStore();

    return (
        <div className="relative">
            {props.children}

            {store.activeTutorialStep ? (
                <>
                    <div className="h-20"></div>

                    {store.activeTutorialStep.itemId ? (
                        <div className="fixed inset-0 bg-black bg-opacity-20" />
                    ) : null}

                    <div className="fixed left-0 right-0 bottom-8 mx-auto w-[480px] bg-white p-4 rounded-xl border-2 border-gray-100 dark:border-gray-800 overflow-clip shadow-lg">
                        <div className="flex flex-col gap-3 w-full h-full">
                            <div className="flex flex-row items-start justify-between gap-2">
                                <div className="mt-0.5 ml-0.5">{store.activeTutorialStep.body}</div>

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
                                    <MaterialSymbol icon="close" className="text-2xl" />
                                </TutorialOverlayButton>
                            </div>

                            <div className="flex flex-row justify-between">
                                {store.activeTutorialStep.backButton !== "hidden" ? (
                                    <TutorialOverlayButton
                                        onClick={store.activeTutorialStep.onBack}
                                    >
                                        <div className="flex flex-row items-center gap-1 px-1">
                                            <MaterialSymbol
                                                icon="arrow_back_ios"
                                                className="text-xl w-4"
                                            />
                                            Back
                                        </div>
                                    </TutorialOverlayButton>
                                ) : (
                                    <div />
                                )}

                                {store.activeTutorialStep.continueButton !== "hidden" ? (
                                    <TutorialOverlayButton
                                        onClick={store.activeTutorialStep.onContinue}
                                    >
                                        <div className="flex flex-row items-center gap-1 px-1">
                                            Continue
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

export const TutorialItem = (
    props: React.PropsWithChildren<{ id: string; className?: string }>,
) => {
    const [store, _setStore] = useStore();

    const { refs, floatingStyles } = useFloating({
        open: store.activeTutorialStep?.itemId === props.id,
        middleware: [
            offset(({ rects }) => {
                return -rects.reference.height / 2 - rects.floating.height / 2;
            }),
            size({
                apply: ({ elements }) => {
                    const { width, height } = elements.reference.getBoundingClientRect();
                    elements.floating.style.width = `${width}px`;
                    elements.floating.style.height = `${height}px`;
                },
            }),
        ],
    });

    return (
        <>
            <div ref={refs.setReference} className={props.className}>
                {props.children}
            </div>

            {store.activeTutorialStep?.itemId === props.id ? (
                <FloatingPortal>
                    <div
                        ref={refs.setFloating}
                        style={floatingStyles}
                        className={props.className}
                        onClick={store.activeTutorialStep.onClickItem}
                    >
                        {props.children}
                    </div>
                </FloatingPortal>
            ) : null}
        </>
    );
};
