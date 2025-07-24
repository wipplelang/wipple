import type { Component } from "svelte";

type Widget<Props> = new () => HTMLElement & Props;

export type WidgetProps<W extends Widget<unknown>> = W extends Widget<infer Props> ? Props : never;

export const widget = <Props extends Record<string, any>>(component: Component<Props>) =>
    component.element! as Widget<Props>;
