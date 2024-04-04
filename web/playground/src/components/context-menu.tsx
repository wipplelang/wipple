import {
    FloatingPortal,
    autoUpdate,
    useClick,
    useDismiss,
    useFloating,
    useInteractions,
} from "@floating-ui/react";
import { useEffect, useState } from "react";
import { MaterialSymbol, MaterialSymbolProps } from "react-material-symbols";
import { Tooltip, Transition, defaultAnimationDuration } from ".";

export interface ContextMenuItem {
    title: string | ((props: { onDismiss: () => void }) => React.ReactNode);
    shortcut?: { win: string; mac: string };
    icon?: MaterialSymbolProps["icon"];
    role?: "destructive";
    disabled?: boolean;
    onClick?: () => void;
}

export const ContextMenuButton = (props: {
    className?: string;
    disabled?: boolean;
    description?: string;
    items: ContextMenuItem[];
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
        placement: "bottom-start",
    });

    const click = useClick(context);
    const dismiss = useDismiss(context);

    const { getReferenceProps, getFloatingProps } = useInteractions([click, dismiss]);

    useEffect(() => {
        if (props.disabled) {
            setActive(false);
        }
    }, [props.disabled]);

    return (
        <>
            <Tooltip description={props.description} disabled={props.disabled}>
                <span ref={refs.setReference} {...getReferenceProps()} className={props.className}>
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
                            <ContextMenu items={props.items} onDismiss={() => setActive(false)} />
                        </Transition>
                    </div>
                </FloatingPortal>
            ) : null}
        </>
    );
};

const ContextMenu = (props: { items: ContextMenuItem[]; onDismiss: () => void }) => (
    <ul className="flex flex-col items-stretch gap-0.5 bg-white dark:bg-gray-800 p-1 rounded-lg shadow-lg">
        {props.items.map((item, index) => (
            <button
                disabled={item.disabled}
                key={index}
                onClick={() => {
                    if (item.onClick) {
                        item.onClick();
                        props.onDismiss();
                    } else {
                        alert("Click and hold to drag out of the menu.");
                    }
                }}
                className={`flex flex-row items-center gap-1.5 text-sm disabled:opacity-50 rounded-md px-2 py-0.5 transition-colors ${
                    item.role === "destructive"
                        ? "text-red-500 enabled:hover:bg-red-50 enabled:dark:hover:bg-red-950 enabled:dark:hover:bg-opacity-50"
                        : "text-gray-900 dark:text-gray-50 enabled:hover:bg-gray-100 enabled:dark:hover:bg-gray-700"
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
        ))}
    </ul>
);
