import { useMemo } from "react";
import { Tooltip } from "../../../components";
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

export const colors = {
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

export const ColorAsset = (props: {
    color: string;
    tooltip?: boolean;
    disabled?: boolean;
    onClick?: () => void;
}) => {
    const description = useMemo(() => colorName(props.color), [props.color]);

    const color = <div className="w-4 h-4" style={{ backgroundColor: props.color }} />;

    return (
        <div
            className={`inline-block align-text-bottom rounded-md border-[1.5px] border-gray-100 dark:border-gray-800 overflow-clip ${
                props.tooltip ?? true ? "hover:scale-110 transition-transform" : ""
            }`}
        >
            {props.tooltip ?? true ? (
                <Tooltip
                    disabled={props.disabled}
                    description={<span className="capitalize">{description}</span>}
                    onClick={props.onClick}
                >
                    {color}
                </Tooltip>
            ) : (
                color
            )}
        </div>
    );
};

export const colorName = (color: string) => {
    // Return named colors as-is
    if (!color.startsWith("#")) {
        return color;
    }

    // Then try Tailwind color names
    for (const [name, shades] of Object.entries(colors)) {
        if ((Object.values(shades) as string[]).includes(color)) {
            return name;
        }
    }

    // Finally, use the name of the closest named color
    return namer(color, { pick: ["basic"] }).basic[0].name.toLowerCase();
};
