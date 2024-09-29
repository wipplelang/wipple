import { MaterialSymbol, MaterialSymbolProps } from "react-material-symbols";

export interface ButtonProps {
    role: "primary" | "secondary" | "destructive";
    fill?: boolean;
    icon?: MaterialSymbolProps["icon"];
    onClick: () => void;
}

export const Button = (props: React.PropsWithChildren<ButtonProps>) => {
    const fill = props.fill ?? true;

    let backgroundColor: string;
    let textColor: string;
    let fontWeight: string;
    switch (props.role) {
        case "primary":
            backgroundColor = fill
                ? "bg-blue-500 hover:bg-blue-600"
                : "hover:bg-gray-100 dark:hover:bg-gray-900";
            textColor = fill
                ? "text-white"
                : "text-blue-500 hover:text-blue-600 dark:hover:text-blue-400";
            fontWeight = "font-semibold";
            break;
        case "secondary":
            backgroundColor =
                fill ?? true
                    ? "bg-gray-200 hover:bg-gray-300 dark:bg-gray-600 dark:bg-gray-500"
                    : "hover:bg-gray-100 dark:hover:bg-gray-900";
            textColor = fill
                ? "text-gray-900 dark:text-gray-50"
                : "text-gray-500 dark:text-gray-400";
            fontWeight = "font-medium";
            break;
        case "destructive":
            backgroundColor = fill
                ? "bg-red-500 hover:bg-red-600"
                : "hover:bg-gray-100 dark:hover:bg-gray-900";
            textColor = fill
                ? "text-white"
                : "text-red-500 hover:text-red-600 dark:hover:text-red-400";
            fontWeight = "font-semibold";
            break;
    }

    return (
        <button
            className={`flex flex-row items-center justify-center gap-1 px-4 py-2 rounded-md transition-colors ${backgroundColor} ${textColor} ${fontWeight}`}
            onClick={props.onClick}
        >
            {props.icon ? <MaterialSymbol icon={props.icon} className="text-xl" /> : null}
            {props.children}
        </button>
    );
};
