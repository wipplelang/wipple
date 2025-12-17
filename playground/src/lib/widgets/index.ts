import { widget } from "$lib/models/Widget";
import ColorWidget from "./ColorWidget.svelte";
import AnimalWidget from "./AnimalWidget.svelte";
import DropdownWidget from "./DropdownWidget.svelte";
import MusicWidget from "./MusicWidget.svelte";
import NumberWidget from "./NumberWidget.svelte";

const widgets = {
    "animal-widget": widget(AnimalWidget),
    "color-widget": widget(ColorWidget),
    "dropdown-widget": widget(DropdownWidget),
    "music-widget": widget(MusicWidget),
};

// Rendered automatically for numbers with units
widget(NumberWidget);

export type WidgetType = keyof typeof widgets;

export default widgets;
