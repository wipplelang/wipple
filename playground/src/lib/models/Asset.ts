import type widgets from "$lib/widgets";
import type { WidgetType } from "$lib/widgets";
import type { WidgetProps } from "./Widget";

export type Asset = {
    [T in WidgetType]: {
        type: T;
    } & WidgetProps<(typeof widgets)[T]>;
}[WidgetType];

export const stringifyAsset = ({ type, ...props }: Asset) =>
    `${type} '${JSON.stringify(props).replaceAll("'", "\\'")}'`;
