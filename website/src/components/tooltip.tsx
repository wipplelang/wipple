import React, { useEffect, useState } from "react";
import { Transition, defaultAnimationDuration } from ".";
import { useDebounceValue, useResizeObserver } from "usehooks-ts";
import {
    safePolygon,
    useFloating,
    useHover,
    useDismiss,
    useInteractions,
    FloatingPortal,
    useClick,
} from "@floating-ui/react";

export const Tooltip = (
    props: React.PropsWithChildren<{
        description: React.ReactNode;
        content?: (props: { dismiss: () => void }) => React.ReactNode;
        onClick?: (open: boolean) => void;
    }>,
) => {
    const [isHovering, setHovering] = useState(false);
    const [isExpanded, setExpanded] = useState(false);

    const [debouncedHovering, setDebouncedHovering] = useDebounceValue(isHovering, 300);

    useEffect(() => {
        setDebouncedHovering(isHovering);
    }, [isHovering]);

    const { refs, floatingStyles, context } = useFloating({
        open: isHovering,
        onOpenChange: (open, _event, reason) => {
            setHovering(open);
            setExpanded(reason === "click");

            if (reason === "click") {
                props.onClick?.(open);
            }
        },
        placement: "bottom",
    });

    const hover = useHover(context, {
        handleClose: safePolygon({ buffer: 5 }),
    });

    const click = useClick(context, {
        enabled: props.content != null,
    });

    const dismiss = useDismiss(context);

    const { getReferenceProps, getFloatingProps } = useInteractions([hover, click, dismiss]);

    const { width } = useResizeObserver({
        ref: refs.domReference as React.MutableRefObject<HTMLDivElement>,
    });

    return (
        <>
            <span ref={refs.setReference} {...getReferenceProps()}>
                {props.children}
            </span>

            {isHovering || debouncedHovering ? (
                <FloatingPortal>
                    <div ref={refs.setFloating} style={floatingStyles} {...getFloatingProps()}>
                        <div style={{ width, marginTop: 4 }}>
                            <TooltipContent open={isHovering && debouncedHovering}>
                                {isExpanded && props.content != null ? (
                                    <props.content
                                        dismiss={() => {
                                            setHovering(false);
                                            setExpanded(false);
                                            props.onClick?.(false);
                                        }}
                                    />
                                ) : (
                                    props.description
                                )}
                            </TooltipContent>
                        </div>
                    </div>
                </FloatingPortal>
            ) : null}
        </>
    );
};

const TooltipContent = (props: { open: boolean; children: React.ReactNode }) => {
    return (
        <div className="flex items-center justify-center w-full">
            <Transition
                value={props.open ? {} : undefined}
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
    );
};
