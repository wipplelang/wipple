import { useMemo } from "react";
import { Tooltip, useAlert } from "../../../components";
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

export const ColorAsset = (props: { color: string; disabled?: boolean; onClick: () => void }) => {
    const description = useMemo(() => colorName(props.color), [props.color]);

    return (
        <div className="inline-block align-text-bottom rounded-md border-2 border-gray-100 dark:border-gray-800 overflow-clip hover:scale-110 transition-transform">
            <Tooltip
                disabled={props.disabled}
                description={<span className="capitalize">{description}</span>}
                onClick={props.onClick}
            >
                <div className="w-4 h-4" style={{ backgroundColor: props.color }} />
            </Tooltip>
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
