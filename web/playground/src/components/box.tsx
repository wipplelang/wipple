import { useStore } from "../store";

export const Box = (props: {
    children: React.ReactNode;
    fill?: boolean;
    shadow?: boolean;
    showBorderWhenPrinting?: boolean;
}) => {
    const [store, _setStore] = useStore();

    return (
        <div
            className={`relative flex flex-col h-full bg-white dark:bg-gray-900 rounded-lg ${
                props.fill ? "flex-1" : ""
            } ${
                props.shadow ?? true ? "data-[shadow]:shadow-md data-[shadow]:shadow-black/5" : ""
            } ${store.isPrinting ? "" : "overflow-y-scroll"}  ${
                !store.isPrinting || props.showBorderWhenPrinting
                    ? "p-3 border-[1.5px] border-gray-100 dark:border-gray-800"
                    : ""
            }`}
        >
            {props.children}
        </div>
    );
};
