import { useEffect, useRef, useState } from "react";
import { Transition, defaultAnimationDuration } from ".";
import { createPortal } from "react-dom";
import { useDebounceValue } from "usehooks-ts";

export const Tooltip = (props: React.PropsWithChildren<{ description: React.ReactNode }>) => {
    const childrenRef = useRef<HTMLSpanElement>(null);

    const [isHovering, setHovering] = useState(false);

    const [debouncedHovering, setDebouncedHovering] = useDebounceValue(isHovering, 300);

    useEffect(() => {
        setDebouncedHovering(isHovering);
    }, [isHovering]);

    return (
        <>
            <span
                ref={childrenRef}
                onMouseEnter={() => setHovering(true)}
                onMouseLeave={() => setHovering(false)}
            >
                {props.children}
            </span>

            {isHovering
                ? createPortal(
                      <TooltipContent
                          relativeTo={childrenRef}
                          isHovering={isHovering && debouncedHovering}
                      >
                          {props.description}
                      </TooltipContent>,
                      document.body,
                  )
                : null}
        </>
    );
};

const TooltipContent = (props: {
    relativeTo: React.RefObject<HTMLElement>;
    isHovering: boolean;
    interactive?: boolean;
    children: React.ReactNode;
}) => {
    const [{ top, left, right, width, height }, setPosition] = useState({
        top: 0,
        left: 0,
        right: 0,
        width: 0,
        height: 0,
    });

    useEffect(() => {
        if (!props.relativeTo.current) return;

        const rect = props.relativeTo.current.getBoundingClientRect();
        setPosition(rect);
    }, [props.relativeTo.current, props.isHovering]);

    return (
        <div
            className={`fixed mt-1 ${props.interactive ? "" : "pointer-events-none"}`}
            style={{ top: top + height, left, right, width }}
        >
            <div className="flex items-center justify-center w-full">
                <Transition
                    value={props.isHovering ? {} : undefined}
                    exitAnimationDuration={defaultAnimationDuration}
                    inClassName="animate-in zoom-in-95 fade-in-25"
                    outClassName="animate-out zoom-out-95 fade-out-25"
                >
                    {() => (
                        <div className="border border-gray-50 dark:border-gray-900 bg-white dark:bg-gray-800 px-2.5 py-1 rounded-xl shadow-lg shadow-gray-100 dark:shadow-gray-950 text-gray-600 dark:text-gray-400 text-sm">
                            {typeof props.children === "string" ? (
                                <p className="whitespace-nowrap">{props.children}</p>
                            ) : (
                                props.children
                            )}
                        </div>
                    )}
                </Transition>
            </div>
        </div>
    );
};
