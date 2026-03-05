<script lang="ts">
    import * as Tonal from "tonal";
    import Box from "./Box.svelte";
    import { notes, noteSymbol, type Music } from "$lib/assets/music";
    import PianoKey, { pianoKeyHandleWidth, pianoKeyWidthMultiplier } from "./PianoKey.svelte";
    import Tooltip from "./Tooltip.svelte";

    interface Props {
        music: Music;
        activeNoteIndex?: number;
        onplaynote: (note: string) => void;
    }

    let { music = $bindable(), activeNoteIndex, onplaynote }: Props = $props();

    const keyWidth = 80;
    const noteHeight = 20;
    const visibleNotes = 28;
    const offsetNotes = 24;

    let container: HTMLDivElement;
    $effect(() => {
        requestAnimationFrame(() => {
            container.scrollTo({
                top: noteHeight * offsetNotes,
                behavior: "instant",
            });
        });
    });

    const toggle = (note: string, index: number) => {
        const existingNoteIndex = music.notes[index].notes.indexOf(note);
        if (existingNoteIndex !== -1) {
            music.notes[index].notes.splice(existingNoteIndex, 1);
        } else {
            music.notes[index].notes.push(note);
            onplaynote(note);
        }
    };

    const setDuration = (duration: number, index: number) => {
        music.notes[index].duration = duration;
    };
</script>

<Box>
    <div
        bind:this={container}
        class="relative max-h-[calc(var(--note-height)*var(--visible-notes))] overflow-scroll"
        style:--note-height="{noteHeight}px"
        style:--visible-notes={visibleNotes}
    >
        <div
            class="bg-background-secondary-alt sticky top-0 flex w-fit flex-row justify-start border-b-[1px] border-neutral-500/10"
        >
            <div style:min-width="{keyWidth}px"></div>

            {#each music.notes as { duration }, index (index)}
                <div
                    class="flex flex-row items-center gap-[8px] data-[active]:bg-blue-500/20"
                    data-active={index === activeNoteIndex || undefined}
                    style:min-width="{duration * pianoKeyWidthMultiplier}px"
                >
                    <div class="flex-1 border-b-2 border-neutral-500/10"></div>

                    <Tooltip content="{duration} {duration === 1 ? 'beat' : 'beats'}">
                        <p class="music-symbols text-sm text-neutral-500/50">
                            {noteSymbol(duration)}
                        </p>
                    </Tooltip>

                    <div class="flex-1 border-b-2 border-neutral-500/10"></div>
                </div>

                <div style:min-width="{pianoKeyHandleWidth}px"></div>
            {/each}
        </div>

        <div class="flex flex-col-reverse items-start">
            {#each notes[music.type] as note, index (index)}
                <div
                    class="flex h-(--note-height) flex-row justify-start *:border-y-[1px] *:border-r-[1.5px] *:border-neutral-500/10"
                >
                    {@render key(note)}

                    {#each music.notes as { duration, notes }, index (index)}
                        <PianoKey
                            {duration}
                            {note}
                            on={notes.includes(note)}
                            onclick={() => toggle(note, index)}
                            onchangeduration={(duration) => setDuration(duration, index)}
                        />
                    {/each}
                </div>
            {/each}
        </div>
    </div>
</Box>

{#snippet key(note: string)}
    {@const { letter, acc } = Tonal.Note.get(note)}

    <button
        onclick={() => onplaynote(note)}
        class={[
            "px-[4px] text-right text-xs",
            acc
                ? "bg-black text-white hover:bg-neutral-700"
                : "bg-white text-black hover:bg-neutral-200",
        ]}
        style:width="{keyWidth}px"
    >
        {letter ? `${letter}${acc}` : note}
    </button>
{/snippet}
