export interface ButtonProps {
    role: "primary" | "secondary" | "destructive";
    onClick: () => void;
}

export const Button = (props: React.PropsWithChildren<ButtonProps>) => {
    let backgroundColor: string;
    let textColor: string;
    let fontWeight: string;
    switch (props.role) {
        case "primary":
            backgroundColor = "bg-sky-500 hover:bg-sky-600";
            textColor = "text-white";
            fontWeight = "font-semibold";
            break;
        case "secondary":
            backgroundColor = "bg-gray-200 hover:bg-gray-300 dark:bg-gray-600 dark:bg-gray-500";
            textColor = "text-gray-900 dark:text-gray-50";
            fontWeight = "font-medium";
            break;
        case "destructive":
            backgroundColor = "bg-red-500 hover:bg-red-600";
            textColor = "text-white";
            fontWeight = "font-semibold";
            break;
    }

    return (
        <button
            className={`px-4 py-2 rounded-md ${backgroundColor} ${textColor} ${fontWeight}`}
            onClick={props.onClick}
        >
            {props.children}
        </button>
    );
};
