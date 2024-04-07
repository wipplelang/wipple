import { ColorAsset } from "./color";
import { DropdownAsset } from "./dropdown";

export const isAsset = (value: string) => getAsset(value) != null;

export type Asset =
    | { type: "color"; color: string }
    | { type: "dropdown"; selection: string; options: string[] };

export const getAsset = (code: string): Asset | undefined => {
    const split = code.split(" ");

    let [type, value] = split;
    type ??= "";
    value ??= "";
    value += split.slice(2).length > 0 ? " " + split.slice(2).join(" ") : "";

    switch (type) {
        case "Color": {
            value = value.slice(1, value.length - 1); // remove quotes
            return { type: "color", color: value };
        }
        case "Dropdown": {
            const typeMatch = value.match(/\((?<options>.+)\) (?<selection>.+)/);

            if (!typeMatch?.groups) {
                return undefined;
            }

            const options = typeMatch.groups["options"].split(" , ");
            const selection = typeMatch.groups["selection"];

            return { type: "dropdown", selection, options };
        }
        default: {
            return undefined;
        }
    }
};

export const colorAsset = (color: string) => `[Color "${color}"]`;

export const dropdownAsset = (selection: string, options: string[]) =>
    `[Dropdown (${options.join(" , ")}) ${selection}]`;

export const Asset = (props: {
    children: Asset;
    disabled?: boolean;
    onClick?: (asset: Asset) => void;
}) => {
    const asset = props.children;

    let content: JSX.Element;
    switch (asset.type) {
        case "color": {
            content = (
                <ColorAsset
                    disabled={props.disabled}
                    color={asset.color}
                    onClick={() => props.onClick?.(asset)}
                />
            );
            break;
        }
        case "dropdown": {
            content = (
                <DropdownAsset
                    disabled={props.disabled}
                    value={asset.selection}
                    options={asset.options}
                    onChange={(selection) => props.onClick?.({ ...asset, selection })}
                />
            );

            break;
        }
        default: {
            return null;
        }
    }

    return (
        <button className={`inline-block w-fit ${props.disabled ? "opacity-50" : ""}`}>
            {content}
        </button>
    );
};
