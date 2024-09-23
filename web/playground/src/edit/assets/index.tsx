import { AnimalAsset } from "./animal";
import { ColorAsset } from "./color";
import { DropdownAsset } from "./dropdown";
import { MelodyAsset } from "./melody";
import { ObjectAsset } from "./object";
import { RhythmAsset } from "./rhythm";
import { SliderAsset } from "./slider";

export const isAsset = (value: string) =>
    value.startsWith("[") &&
    value.endsWith("]") &&
    getAsset(value.slice(1, value.length - 1)) != null;

export type Asset =
    | { type: "color"; color: string }
    | { type: "animal"; animal: string }
    | { type: "dropdown"; selection: string; options: string[] }
    | { type: "melody"; melody: string }
    | { type: "rhythm"; rhythm: string }
    | { type: "object"; object: string }
    | { type: "slider"; value: number; min: number; max: number };

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
        case "Melody": {
            value = value.slice(1, value.length - 1); // remove quotes
            return { type: "melody", melody: value };
        }
        case "Rhythm": {
            value = value.slice(1, value.length - 1); // remove quotes
            return { type: "rhythm", rhythm: value };
        }
        case "Object": {
            value = value.slice(1, value.length - 1); // remove quotes
            return { type: "object", object: value };
        }
        case "Slider": {
            const typeMatch = value.match(/(?<current>[\d\.]+) (?<min>[\d\.]+) (?<max>[\d\.]+)/);

            if (!typeMatch?.groups) {
                return undefined;
            }

            const current = parseFloat(typeMatch.groups["current"]);
            const min = parseFloat(typeMatch.groups["min"]);
            const max = parseFloat(typeMatch.groups["max"]);
            return { type: "slider", value: current, min, max };
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

export const melodyAsset = (melody: string) => `[Melody "${melody}"]`;

export const rhythmAsset = (rhythm: string) => `[Rhythm "${rhythm}"]`;

export const objectAsset = (object: string) => `[Object "${object}"]`;

export const sliderAsset = (value: number, min: number, max: number) =>
    `[Slider ${value} ${min} ${max}]`;

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
        case "melody": {
            content = (
                <MelodyAsset
                    disabled={props.disabled}
                    melody={asset.melody}
                    onClick={() => props.onClick?.(asset)}
                />
            );
            break;
        }
        case "rhythm": {
            content = (
                <RhythmAsset
                    disabled={props.disabled}
                    rhythm={asset.rhythm}
                    onClick={() => props.onClick?.(asset)}
                />
            );
            break;
        }
        case "object": {
            content = (
                <ObjectAsset
                    disabled={props.disabled}
                    object={asset.object}
                    onClick={() => props.onClick?.(asset)}
                />
            );
            break;
        }
        case "slider": {
            content = (
                <SliderAsset
                    disabled={props.disabled}
                    value={asset.value}
                    min={asset.min}
                    max={asset.max}
                    onChange={(value) => props.onClick?.({ ...asset, value })}
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
