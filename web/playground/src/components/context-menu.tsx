import {
    FloatingPortal,
    autoUpdate,
    useClick,
    useDismiss,
    useFloating,
    useInteractions,
    autoPlacement,
} from "@floating-ui/react";
import { useCallback, useEffect, useState } from "react";
import { MaterialSymbol, MaterialSymbolProps } from "react-material-symbols";
import { Tooltip, Transition, defaultAnimationDuration } from ".";

export interface ContextMenuItem {
    title: string | ((props: { onDismiss: () => void }) => React.ReactNode);
    shortcut?: { win: string; mac: string };
    icon?: MaterialSymbolProps["icon"];
    divider?: boolean;
    highlight?: boolean;
    role?: "destructive";
    disabled?: boolean;
    onClick?: () => void;
}

export const ContextMenuButton = (props: {
    className?: string;
    disabled?: boolean;
    description?: string;
    items: (ContextMenuItem | undefined)[];
    children: React.ReactNode;
}) => {
    const [isActive, setActive] = useState(false);
    const [isVisible, setVisible] = useState(false);

    useEffect(() => {
        if (isActive) {
            setVisible(true);
        } else {
            setTimeout(() => {
                setVisible(false);
            }, defaultAnimationDuration);
        }
    }, [isActive]);

    const { refs, floatingStyles, context } = useFloating({
        open: isActive,
        onOpenChange: setActive,
        whileElementsMounted: autoUpdate,
        middleware: [
            autoPlacement({
                allowedPlacements: ["top-start", "top-end", "bottom-start", "bottom-end"],
            }),
        ],
    });

    const click = useClick(context);
    const dismiss = useDismiss(context);

    const { getReferenceProps, getFloatingProps } = useInteractions([click, dismiss]);

    useEffect(() => {
        if (props.disabled) {
            setActive(false);
        }
    }, [props.disabled]);

    const handleDismiss = useCallback(() => {
        setActive(false);
    }, []);

    return (
        <>
            <Tooltip description={props.description} disabled={props.disabled}>
                <span
                    ref={refs.setReference}
                    {...getReferenceProps({
                        onClick: (e) => {
                            e.preventDefault();
                            e.stopPropagation();
                        },
                    })}
                    className={props.className}
                >
                    {props.children}
                </span>
            </Tooltip>

            {isVisible ? (
                <FloatingPortal>
                    <div
                        ref={refs.setFloating}
                        style={floatingStyles}
                        {...getFloatingProps()}
                        className="z-20"
                    >
                        <Transition
                            in={isActive}
                            animateOnMount
                            inStyle={{ opacity: 1, y: 0 }}
                            outStyle={{ opacity: 0, y: "-0.25rem" }}
                        >
                            <ContextMenu items={props.items} onDismiss={handleDismiss} />
                        </Transition>
                    </div>
                </FloatingPortal>
            ) : null}
        </>
    );
};

const ContextMenu = (props: { items: (ContextMenuItem | undefined)[]; onDismiss: () => void }) => (
    <ul className="bg-white dark:bg-gray-800 rounded-lg shadow-lg">
        <ContextMenuContent items={props.items} onDismiss={props.onDismiss} />
    </ul>
);

export const ContextMenuContent = (props: {
    items: (ContextMenuItem | undefined)[];
    onDismiss: () => void;
}) => (
    <ul className="flex flex-col items-stretch gap-0.5 p-1 h-full max-h-[400px] overflow-y-scroll">
        {props.items.map((item, index) =>
            item ? (
                <div
                    key={index}
                    className={
                        item.divider
                            ? "mb-0.5 pb-0.5 border-b-2 border-b-gray-100 dark:border-b-gray-900"
                            : ""
                    }
                >
                    <button
                        disabled={item.disabled}
                        onClick={(e) => {
                            e.preventDefault();
                            e.stopPropagation();

                            if (item.onClick) {
                                item.onClick();
                                props.onDismiss();
                            } else {
                                alert("Click and hold to drag out of the menu.");
                            }
                        }}
                        className={`flex flex-row items-center gap-1.5 text-sm disabled:opacity-50 rounded-md px-2 py-0.5 transition-colors w-full ${
                            item.role === "destructive"
                                ? `text-red-500 ${
                                      item.highlight ?? true
                                          ? "enabled:hover:bg-red-50 enabled:dark:hover:bg-red-950 enabled:dark:hover:bg-opacity-50"
                                          : ""
                                  }`
                                : `text-gray-900 ${
                                      item.highlight ?? true
                                          ? "dark:text-gray-50 enabled:hover:bg-gray-100 enabled:dark:hover:bg-gray-700"
                                          : ""
                                  }`
                        }`}
                    >
                        {item.icon ? <MaterialSymbol icon={item.icon} className="text-lg" /> : null}

                        {typeof item.title === "string" ? (
                            <p>{item.title}</p>
                        ) : (
                            <item.title onDismiss={props.onDismiss} />
                        )}

                        <div className="flex-1" />

                        {item.shortcut ? (
                            <p className="text-sm opacity-50">
                                {/mac/.test(navigator.userAgent.toLowerCase())
                                    ? item.shortcut.mac
                                    : item.shortcut.win}
                            </p>
                        ) : null}
                    </button>
                </div>
            ) : null,
        )}
    </ul>
);
