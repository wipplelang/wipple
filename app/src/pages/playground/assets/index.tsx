import { ColorAsset } from "./color";

export const isAsset = (value: string) => value.startsWith("[") && value.endsWith("]");

export const colorAsset = (color: string) => `[color "${color}"]`;

export const Asset = (props: {
    children: string;
    disabled?: boolean;
    onClick?: (type: string, value: string) => void;
}) => {
    let [type, value] = props.children.split(" ", 2);
    type ??= "";
    value ??= "";

    if (value) {
        value = value.slice(1, value.length - 1); // remove quotes
    }

    let content: JSX.Element;
    switch (type) {
        case "color":
            content = (
                <ColorAsset
                    disabled={props.disabled}
                    color={value}
                    onClick={() => props.onClick?.("color", value)}
                />
            );
            break;
        default:
            content = <UnknownAsset value={value} />;
            break;
    }

    return (
        <button
            className={`inline-block align-text-bottom w-fit ${
                props.disabled ? "opacity-50" : "hover:scale-110 transition-transform"
            }`}
            disabled={props.disabled}
            onClick={() => props.onClick?.(type, value)}
        >
            <div className="rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip">
                {content}
            </div>
        </button>
    );
};

const UnknownAsset = (props: { value: string }) => (
    <div className="ui-font w-4 h-4">{props.value}</div>
);
