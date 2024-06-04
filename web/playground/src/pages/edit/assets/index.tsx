import { AnimalAsset } from "./animal";
import { ColorAsset } from "./color";
import { DropdownAsset } from "./dropdown";
import { InstrumentAsset } from "./instrument";
import { NoteAsset } from "./note";

export const isAsset = (value: string) =>
    value.startsWith("[") &&
    value.endsWith("]") &&
    getAsset(value.slice(1, value.length - 1)) != null;

export type Asset =
    | { type: "color"; color: string }
    | { type: "animal"; animal: string }
    | { type: "dropdown"; selection: string; options: string[] }
    | { type: "note"; note: string }
    | { type: "instrument"; instrument: string };

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
        case "Animal": {
            value = value.slice(1, value.length - 1); // remove quotes
            return { type: "animal", animal: value };
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
        case "Note": {
            value = value.slice(1, value.length - 1); // remove quotes
            return { type: "note", note: value };
        }
        case "Instrument-Name": {
            value = value.slice(1, value.length - 1); // remove quotes
            return { type: "instrument", instrument: value };
        }
        default: {
            return undefined;
        }
    }
};

export const colorAsset = (color: string) => `[Color "${color}"]`;

export const animalAsset = (animal: string) => `[Animal "${animal}"]`;

export const dropdownAsset = (selection: string, options: string[]) =>
    `[Dropdown (${options.join(" , ")}) ${selection}]`;

export const noteAsset = (note: string) => `[Note "${note}"]`;

export const instrumentAsset = (instrument: string) => `[Instrument-Name "${instrument}"]`;

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
        case "animal": {
            content = (
                <AnimalAsset
                    disabled={props.disabled}
                    animal={asset.animal}
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
        case "note": {
            content = (
                <NoteAsset
                    disabled={props.disabled}
                    note={asset.note}
                    onClick={() => props.onClick?.(asset)}
                />
            );
            break;
        }
        case "instrument": {
            content = (
                <InstrumentAsset
                    disabled={props.disabled}
                    instrument={asset.instrument}
                    onClick={() => props.onClick?.(asset)}
                />
            );
            break;
        }
        default: {
            asset satisfies never;
            return null;
        }
    }

    return (
        <button
            className={`inline-flex w-fit items-center align-middle h-[calc(1lh-2pt)] ${
                props.disabled ? "opacity-50" : ""
            }`}
        >
            {content}
        </button>
    );
};
