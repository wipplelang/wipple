import React, { useEffect, useState } from "react";
import { Transition, defaultAnimationDuration } from ".";
import { useDebounceValue } from "usehooks-ts";
import {
    safePolygon,
    useFloating,
    useHover,
    useDismiss,
    useInteractions,
    FloatingPortal,
    useClick,
    shift,
    autoUpdate,
} from "@floating-ui/react";

export const Tooltip = (
    props: React.PropsWithChildren<{
        disabled?: boolean;
        description: React.ReactNode;
        onClick?: () => void;
    }>,
) => {
    const [isHovering, setHovering] = useState(false);

    const [debouncedHovering, setDebouncedHovering] = useDebounceValue(isHovering, 300);

    useEffect(() => {
        setDebouncedHovering(isHovering);
    }, [isHovering]);

    const { refs, floatingStyles, context } = useFloating({
        open: isHovering,
        whileElementsMounted: autoUpdate,
        onOpenChange: (open, _event, reason) => {
            if (props.disabled ?? false) {
                return;
            }

            setHovering(open);

            if (reason === "click") {
                props.onClick?.();
            }
        },
        placement: "bottom",
        middleware: [shift({ padding: 8 })],
    });

    const hover = useHover(context, {
        handleClose: safePolygon({ buffer: 5 }),
    });

    const click = useClick(context);

    const dismiss = useDismiss(context);

    const { getReferenceProps, getFloatingProps } = useInteractions([hover, click, dismiss]);

    return (
        <>
            <span ref={refs.setReference} {...getReferenceProps()}>
                {props.children}
            </span>

            {isHovering || debouncedHovering ? (
                <FloatingPortal>
                    <div ref={refs.setFloating} style={floatingStyles} {...getFloatingProps()}>
                        <div style={{ marginTop: 4 }}>
                            <TooltipContent open={isHovering && debouncedHovering}>
                                {props.description}
                            </TooltipContent>
                        </div>
                    </div>
                </FloatingPortal>
            ) : null}
        </>
    );
};

const TooltipContent = (props: { open: boolean; children: React.ReactNode }) => (
    <div className="flex items-center justify-center w-fit">
        <Transition
            in={props.open}
            exitAnimationDuration={defaultAnimationDuration}
            inClassName="animate-in zoom-in-95 fade-in-25"
            outClassName="animate-out zoom-out-95 fade-out-25"
        >
            <div className="border border-gray-50 dark:border-gray-900 bg-white dark:bg-gray-800 px-2.5 py-1 rounded-xl shadow-lg shadow-gray-100 dark:shadow-gray-950 text-gray-600 dark:text-gray-400 text-sm">
                {typeof props.children === "string" ? (
                    <p className="whitespace-nowrap">{props.children}</p>
                ) : (
                    props.children
                )}
            </div>
        </Transition>
    </div>
);
