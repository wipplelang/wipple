import type widgets from "@/widgets";
import type { WidgetType } from "@/widgets";
import type { WidgetProps } from "./Widget";

export type Asset = {
    [T in WidgetType]: {
        type: T;
    } & WidgetProps<(typeof widgets)[T]>;
}[WidgetType];

export const stringifyAsset = ({ type, ...props }: Asset) =>
    `${type} '${JSON.stringify(props).replaceAll("'", "\\'")}'`;
