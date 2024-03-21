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
    title: string;
    icon: MaterialSymbolProps["icon"];
    role?: "destructive";
    disabled?: boolean;
    onClick: () => void;
}

export const ContextMenuButton = (props: {
    className?: string;
    disabled?: boolean;
    description: string;
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
                            exitAnimationDuration={defaultAnimationDuration}
                            inClassName="animate-in fade-in-50 slide-in-from-top-1"
                            outClassName="animate-out fade-out-50 slide-out-to-top-1"
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
    <ul className="flex flex-col items-stretch gap-0.5 bg-white dark:bg-gray-800 p-1 rounded-md shadow-lg">
        {props.items.map((item, index) => (
            <button
                disabled={item.disabled}
                key={index}
                onClick={() => {
                    item.onClick();
                    props.onDismiss();
                }}
                className={`flex flex-row items-center gap-1.5 disabled:opacity-50 rounded-sm px-1 py-0.5 transition-colors ${
                    item.role === "destructive"
                        ? "text-red-500 enabled:hover:bg-red-50 enabled:dark:hover:bg-red-950 enabled:dark:hover:bg-opacity-50"
                        : "text-gray-900 dark:text-gray-50 enabled:hover:bg-gray-100 enabled:dark:hover:bg-gray-700"
                }`}
            >
                <MaterialSymbol icon={item.icon} />
                <p className="text-sm">{item.title}</p>
            </button>
        ))}
    </ul>
);
