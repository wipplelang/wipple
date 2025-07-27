<script lang="ts">
    import Box from "$lib/components/Box.svelte";
    import { onMount } from "svelte";
    // @ts-expect-error
    import RealTurtle from "real-turtle";
    import turtleImage from "$lib/assets/turtle.png";
    import { animalSvgUrl } from "$lib/assets/animals";
    import colors from "$lib/assets/colors";

    const canvasSize = 400;
    const canvasPixelRatio = 2.5;

    let canvas: HTMLCanvasElement;
    let turtle: RealTurtle;

    const initializeTurtle = async (canvasEl: HTMLCanvasElement) => {
        const turtle = new RealTurtle(canvasEl, {
            async: true,
            image: turtleImage,
            state: {
                size: 30,
            },
        });

        await turtle.setLineWidth(2);
        await turtle.setPosition(canvasSize / 2, canvasSize / 2);

        return turtle;
    };

    const rescaleCanvas = (canvas: HTMLCanvasElement) => {
        const ctx = canvas.getContext("2d")!;
        canvas.width = canvasSize * canvasPixelRatio;
        canvas.height = canvasSize * canvasPixelRatio;
        ctx.scale(canvasPixelRatio, canvasPixelRatio);
    };

    const reset = async () => {
        canvas.style.opacity = "0";
        rescaleCanvas(canvas);
        turtle = await initializeTurtle(canvas);
        canvas.style.opacity = "1";
    };

    onMount(async () => {
        await reset();
    });

    export const _initialize = async () => {
        await reset();
    };

    export const forward = async (distance: number) => {
        await turtle?.forward(distance);
    };

    export const backward = async (distance: number) => {
        await turtle?.back(distance);
    };

    export const arc = async ([radius, angle]: [number, number]) => {
        await turtle?.arc(radius, angle);
    };

    export const left = async (angle: number) => {
        await turtle?.left(angle);
    };

    export const right = async (angle: number) => {
        await turtle?.right(angle);
    };

    export const color = async (colorJson: string) => {
        const { color: colorName } = JSON.parse(colorJson);
        const { varName } = colors[colorName as keyof typeof colors];
        const color = getComputedStyle(canvas).getPropertyValue(varName);
        await turtle?.setStrokeStyle(color);
    };

    export const beginPath = async () => {
        await turtle?.beginPath();
    };

    export const endPath = async (fillColorJson: string) => {
        const { fillColor: fillColorName } = JSON.parse(fillColorJson);
        const { varName } = colors[fillColorName as keyof typeof colors];
        const fillColor = getComputedStyle(canvas).getPropertyValue(varName);

        await turtle?.closePath();
        await turtle?.setFillStyle(fillColor);
        await turtle?.fill();
    };

    export const speed = async (speed: number) => {
        await turtle?.setSpeed(speed / 100);
    };

    export const animal = async (animalJson: string) => {
        const { animal } = JSON.parse(animalJson);
        await turtle?.setImage(animalSvgUrl(animal));
    };
</script>

<div class="data-[printing]:size-[5in]">
    <Box class="relative aspect-square">
        <canvas
            bind:this={canvas}
            width={canvasSize}
            height={canvasSize}
            class="absolute inset-0 size-full"
        ></canvas>
    </Box>
</div>
