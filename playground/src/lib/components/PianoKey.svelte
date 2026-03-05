<script lang="ts" module>
    export const pianoKeyWidthMultiplier = 120;
    export const pianoKeyHandleWidth = 8;
</script>

<script lang="ts">
    import { notesPerBeat } from "$lib/assets/music";

    import { onMount } from "svelte";

    interface Props {
        duration: number;
        note: string;
        on: boolean;
        onclick: () => void;
        onchangeduration: (duration: number) => void;
    }

    const { duration, note, on, onclick, onchangeduration }: Props = $props();

    let handle: HTMLDivElement;
    let width = $derived(duration * pianoKeyWidthMultiplier);

    onMount(() => {
        let pointerId: number;
        let startX: number;
        let startWidth: number;

        const onpointerdown = (e: PointerEvent) => {
            pointerId = e.pointerId;
            startX = e.clientX;
            startWidth = width;

            handle.setPointerCapture(pointerId);
            handle.addEventListener("pointermove", onpointermove);
            handle.addEventListener("pointerup", onpointerup);
        };

        const onpointermove = (e: PointerEvent) => {
            const dx = e.clientX - startX;
            const width = startWidth + dx;

            const newDuration =
                Math.min(
                    Math.max(Math.round((width / pianoKeyWidthMultiplier) * notesPerBeat), 1),
                    notesPerBeat * 4,
                ) / notesPerBeat;

            onchangeduration(newDuration);
        };

        const onpointerup = () => {
            if (pointerId != null) {
                handle.releasePointerCapture(pointerId);
            }

            handle.removeEventListener("pointermove", onpointermove);
            handle.removeEventListener("pointerup", onpointerup);
        };

        handle.addEventListener("pointerdown", onpointerdown);

        return () => {
            onpointerup();
            handle.removeEventListener("pointerdown", onpointerdown);
        };
    });
</script>

<button
    aria-label={note}
    {onclick}
    data-on={on || undefined}
    class="hover:bg-neutral-500/10 data-[on]:bg-blue-500 data-[on]:hover:bg-blue-600"
    style:width="{width}px"
></button>

<div
    bind:this={handle}
    class="cursor-col-resize border-y-0! bg-neutral-500/10"
    style:width="{pianoKeyHandleWidth}px"
></div>
