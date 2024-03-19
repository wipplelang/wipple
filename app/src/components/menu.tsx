import { useState } from "react";
import { Transition, defaultAnimationDuration } from ".";
import { MaterialSymbol } from "react-material-symbols";

export interface MenuItem {
    name: string;
    onClick?: () => void;
    children?: MenuItem[];
}

export const MenuContainer = (props: React.PropsWithChildren<{}>) => (
    <div className="flex flex-row items-center text-gray-600 dark:text-gray-400 text-opacity-50 h-7">
        {props.children}
    </div>
);

export const Menu = (props: { items: MenuItem[] }) => {
    const [stack, setStack] = useState<MenuItem[][] | undefined>([props.items]);
    const [wentBack, setWentBack] = useState(false);

    const pushMenuItems = (items: MenuItem[]) => {
        const prevStack = stack!;
        setStack(undefined);
        setWentBack(false);

        setTimeout(() => {
            setStack([...prevStack, items]);
        }, defaultAnimationDuration);
    };

    const popMenuItems = () => {
        const prevStack = stack!;
        setStack(undefined);
        setWentBack(true);

        setTimeout(() => {
            setStack(prevStack.slice(0, prevStack.length - 1));
        }, defaultAnimationDuration);
    };

    const popToRoot = () => {
        setStack(undefined);
        setWentBack(true);

        setTimeout(() => {
            setStack([props.items]);
        }, defaultAnimationDuration);
    };

    return (
        <Transition
            in={stack != null}
            exitAnimationDuration={defaultAnimationDuration}
            inClassName={`animate-in ${
                wentBack ? "slide-in-from-right-4" : "slide-in-from-left-4"
            } fade-in`}
            outClassName={`animate-out ${
                wentBack ? "slide-out-to-left-4" : "slide-out-to-right-4"
            } fade-out`}
        >
            {stack ? (
                <MenuContainer>
                    {stack.length > 1 ? (
                        <MenuButton onClick={popMenuItems}>
                            <MaterialSymbol icon="arrow_back" className="text-xl my-1" />
                        </MenuButton>
                    ) : null}

                    {stack[stack.length - 1].map((item) => (
                        <MenuButton
                            key={item.name}
                            onClick={() => {
                                item.onClick?.();

                                if (item.children) {
                                    pushMenuItems(item.children);
                                } else {
                                    popToRoot();
                                }
                            }}
                        >
                            {item.name}
                        </MenuButton>
                    ))}
                </MenuContainer>
            ) : null}
        </Transition>
    );
};

const MenuButton = (props: React.PropsWithChildren<{ onClick: () => void }>) => (
    <button
        onClick={props.onClick}
        className="flex items-center justify-center h-7 px-1.5 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 dark:bg-gray-900 disabled:opacity-50"
    >
        {props.children}
    </button>
);
