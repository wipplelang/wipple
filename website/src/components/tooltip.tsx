import { useState } from "react";
import useMeasure from "react-use-measure";
import { Transition, defaultAnimationDuration } from ".";

export const Tooltip = (props: React.PropsWithChildren<{ description: string }>) => {
    const [childrenRef, { height }] = useMeasure();
    const [isHovering, setHovering] = useState(false);

    return (
        <div className="relative">
            <div
                ref={childrenRef}
                onMouseEnter={() => setHovering(true)}
                onMouseLeave={() => setHovering(false)}
            >
                {props.children}
            </div>

            <div
                className="pointer-events-none absolute mt-1"
                style={{ top: height, left: 0, right: 0 }}
            >
                <div className="flex items-center justify-center w-full">
                    <Transition
                        value={isHovering ? {} : undefined}
                        exitAnimationDuration={defaultAnimationDuration}
                        inClassName="animate-in zoom-in-95 fade-in-25"
                        outClassName="animate-out zoom-out-95 fade-out-25"
                    >
                        {() => (
                            <div className="border border-gray-50 dark:border-gray-900 bg-white dark:bg-gray-800 px-2.5 py-1 rounded-full shadow-lg shadow-gray-100 dark:shadow-gray-950 text-gray-600 dark:text-gray-400 text-sm">
                                <p className="whitespace-nowrap">{props.description}</p>
                            </div>
                        )}
                    </Transition>
                </div>
            </div>
        </div>
    );
};
