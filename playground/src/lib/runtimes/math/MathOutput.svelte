<script lang="ts" module>
    const defaultMinX = -10;
    const defaultMinY = -10;
    const defaultMaxX = 10;
    const defaultMaxY = 10;
    const defaultResolution = 0.25;
</script>

<script lang="ts">
    import Box from "$lib/components/Box.svelte";
    import { onMount } from "svelte";
    import { getColor } from "$lib/assets/colors";
    import * as Plotly from "plotly.js-basic-dist-min";

    Plotly.setPlotConfig({
        responsive: true,
        autosizable: true,
        displaylogo: false,
        staticPlot: true,
    });

    let container: HTMLDivElement;

    onMount(async () => {
        await reset();
    });

    let data = $state<Partial<Plotly.PlotData>[]>([]);
    let colors = $state<string[]>([]);
    let minX = $state<number>(defaultMinX);
    let minY = $state<number>(defaultMinY);
    let maxX = $state<number>(defaultMaxX);
    let maxY = $state<number>(defaultMaxY);
    let resolution = $state<number>(defaultResolution);

    const layout = $derived<Partial<Plotly.Layout>>({
        uirevision: "true",
        margin: { t: 40, r: 40, b: 40, l: 40, pad: 0 },
        showlegend: false,
        xaxis: { range: [minX, maxX], dtick: 1, ticklabelstep: 5 },
        yaxis: { range: [minY, maxY], dtick: 1, ticklabelstep: 5 },
    });

    $effect(() => {
        Plotly.react(container, $state.snapshot(data) as any, $state.snapshot(layout) as any);
    });

    const reset = async () => {
        data = [];
        colors = [];
        minX = defaultMinX;
        minY = defaultMinY;
        maxX = defaultMaxX;
        maxY = defaultMaxY;
        resolution = defaultResolution;
    };

    export const _initialize = async () => {
        await reset();
    };

    export const setColor = async (colorJson: string) => {
        colors.push(getColor(JSON.parse(colorJson).color));
    };

    export const setMinX = async (value: number) => {
        minX = value;
    };

    export const setMinY = async (value: number) => {
        minY = value;
    };

    export const setMaxX = async (value: number) => {
        maxX = value;
    };

    export const setMaxY = async (value: number) => {
        maxY = value;
    };

    export const setResolution = async (value: number) => {
        resolution = value;
    };

    let currentPlot: number | undefined;

    export const beginPlot = async () => {
        currentPlot = data.length;

        data.push({
            type: "scatter",
            mode: "lines",
            line: { color: colors[currentPlot] ?? "black", width: 3 },
            x: [],
            y: [],
        });

        return [minX, maxX, resolution];
    };

    export const plot = async ([x, y]: [number, number]) => {
        if (currentPlot == null) return;

        (data[currentPlot].x as number[]).push(x);
        (data[currentPlot].y as number[]).push(y);
    };

    export const endPlot = async () => {
        currentPlot = undefined;
    };
</script>

<div class="relative data-[printing]:size-[5in]">
    <Box class="relative aspect-square">
        <div bind:this={container} class="absolute inset-0 size-full"></div>
    </Box>
</div>
