import { useMemo } from "react";
import { Tooltip } from "../../components";
import namer from "color-namer";
import {
    slate,
    gray,
    zinc,
    neutral,
    stone,
    red,
    orange,
    amber,
    yellow,
    lime,
    green,
    emerald,
    teal,
    cyan,
    sky,
    blue,
    indigo,
    violet,
    purple,
    fuchsia,
    pink,
    rose,
} from "tailwindcss/colors";

const colors = {
    slate: slate,
    gray: gray,
    zinc: zinc,
    neutral: neutral,
    stone: stone,
    red: red,
    orange: orange,
    amber: amber,
    yellow: yellow,
    lime: lime,
    green: green,
    emerald: emerald,
    teal: teal,
    cyan: cyan,
    sky: sky,
    blue: blue,
    indigo: indigo,
    violet: violet,
    purple: purple,
    fuchsia: fuchsia,
    pink: pink,
    rose: rose,
};

export const isAsset = (value: string) => value.startsWith("{") && value.endsWith("}");

export const Asset = (props: {
    children: string;
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
            content = <ColorAsset color={value} />;
            break;
        default:
            content = <UnknownAsset value={value} />;
            break;
    }

    return (
        <div
            className="inline-block align-text-bottom w-fit hover:scale-110 transition-transform"
            onClick={() => props.onClick?.(type, value)}
        >
            <div className="rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip">
                {content}
            </div>
        </div>
    );
};

const ColorAsset = (props: { color: string }) => {
    const colorName = useMemo(() => {
        // Return named colors as-is
        if (!props.color.startsWith("#")) {
            return props.color;
        }

        // Then try Tailwind color names
        for (const [name, shades] of Object.entries(colors)) {
            if ((Object.values(shades) as string[]).includes(props.color)) {
                return name;
            }
        }

        // Finally, use the name of the closest named color
        return namer(props.color, { pick: ["basic"] }).basic[0].name.toLowerCase();
    }, [props.color]);

    return (
        <Tooltip description={colorName}>
            <div className="w-4 h-4" style={{ backgroundColor: props.color }} />
        </Tooltip>
    );
};

const UnknownAsset = (props: { value: string }) => (
    <div className="ui-font w-4 h-4">{props.value}</div>
);
