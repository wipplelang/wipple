<script lang="ts">
    import Box from "$lib/components/Box.svelte";
    import { onMount } from "svelte";
    // @ts-expect-error
    import RealTurtle from "real-turtle";
    import turtleImage from "$lib/assets/turtle.png";
    import { animalSvgUrl } from "$lib/assets/animals";
    import { getColor } from "$lib/assets/colors";
    import type { Action } from "svelte/action";
    import { scale } from "svelte/transition";

    const canvasSize = 400;
    const canvasPixelRatio = 2.5;

    let canvas: HTMLCanvasElement;
    let turtle: RealTurtle;

    const initializeTurtle = async (canvas: HTMLCanvasElement) => {
        const turtle = new RealTurtle(canvas, {
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

    export const go = async ([x, y]: [number, number]) => {
        await turtle?.setPosition(x + canvasSize / 2, y + canvasSize / 2);
    };

    export const color = async (colorJson: string) => {
        const { color } = JSON.parse(colorJson);
        await turtle?.setStrokeStyle(getColor(color));
    };

    export const beginPath = async () => {
        await turtle?.beginPath();
    };

    export const endPath = async (fillColorJson: string) => {
        const { fillColor } = JSON.parse(fillColorJson);

        await turtle?.closePath();
        await turtle?.setFillStyle(getColor(fillColor));
        await turtle?.fill();
    };

    export const speed = async (speed: number) => {
        await turtle?.setSpeed(speed / 100);
    };

    export const animal = async (animalJson: string) => {
        const { animal } = JSON.parse(animalJson);
        await turtle?.setImage(animalSvgUrl(animal));
    };

    const portal: Action = (node) => {
        $effect(() => {
            document.body.appendChild(node);

            return () => {
                node.remove();
            };
        });
    };

    const hoverDelay = 500;

    let startTime = $state<number>();
    let hoverActive = $state(false);
    let hoverPosition = $state<{ x: number; y: number; clientX: number; clientY: number }>();

    const onmouseenter = (e: MouseEvent) => {
        startTime = e.timeStamp;

        setTimeout(() => {
            startTime = undefined;
            hoverActive = true;
        }, hoverDelay);
    };

    const onmousemove = (e: MouseEvent) => {
        const { clientX, clientY, offsetX, offsetY } = e;

        hoverPosition = {
            x:
                Math.floor((offsetX * canvas.width) / (canvas.clientWidth * canvasPixelRatio)) -
                canvasSize / 2,
            y:
                Math.floor((offsetY * canvas.height) / (canvas.clientHeight * canvasPixelRatio)) -
                canvasSize / 2,
            clientX,
            clientY,
        };
    };

    const onmouseleave = () => {
        startTime = undefined;
        hoverActive = false;
        hoverPosition = undefined;
    };
</script>

<div class="relative data-[printing]:size-[5in]">
    <Box class="relative aspect-square" {onmouseenter} {onmousemove} {onmouseleave}>
        <canvas
            bind:this={canvas}
            width={canvasSize}
            height={canvasSize}
            class="absolute inset-0 size-full"
        ></canvas>
    </Box>

    {#if hoverActive && hoverPosition}
        <div
            use:portal
            class="pointer-events-none absolute size-0 -translate-x-1/2 translate-y-[4px]"
            style:left="{hoverPosition.clientX}px"
            style:top="{hoverPosition.clientY}px"
            transition:scale={{ duration: 150, start: 0.95 }}
        >
            <code
                class="bg-background-button float-right inline-block rounded-lg p-[2px] text-xs text-nowrap text-white/60 shadow-md"
            >
                (<span class="text-white">{hoverPosition.x}</span> pixels) (<span
                    class="text-white"
                >
                    {hoverPosition.y}
                </span> pixels)
            </code>
        </div>
    {/if}
</div>
