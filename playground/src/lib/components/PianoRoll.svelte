<script lang="ts">
    import * as Tonal from "tonal";
    import Box from "./Box.svelte";
    import { notes, type Music } from "$lib/assets/music";

    interface Props {
        music: Music;
        onplaynote: (note: string) => void;
    }

    let { music = $bindable(), onplaynote }: Props = $props();

    const noteHeight = 20;
    const visibleNotes = 24;
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
        const existingNoteIndex = music.notes[index].indexOf(note);
        if (existingNoteIndex !== -1) {
            music.notes[index].splice(existingNoteIndex, 1);
        } else {
            music.notes[index].push(note);
            onplaynote(note);
        }
    };
</script>

<Box>
    <div
        bind:this={container}
        class="max-h-[calc(var(--note-height)*var(--visible-notes))] overflow-y-scroll"
        style={`--note-height: ${noteHeight}px; --visible-notes: ${visibleNotes}`}
    >
        <div class="flex flex-col-reverse">
            {#each notes[music.type] as note}
                <div
                    class="h-(--note-height) flex w-full flex-row *:border-y-[1px] *:border-r-[1.5px] *:border-neutral-500/10"
                >
                    {@render key(note)}

                    {#each music.notes as beat, index}
                        {@render button(note, beat.includes(note), () => toggle(note, index))}
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
            "w-[80px] px-[4px] text-right text-xs",
            acc
                ? "bg-black text-white hover:bg-neutral-700"
                : "bg-white text-black hover:bg-neutral-200",
        ]}
    >
        {letter ? `${letter}${acc}` : note}
    </button>
{/snippet}

{#snippet button(note: string, on: boolean, onclick: () => void)}
    <button
        aria-label={note}
        {onclick}
        data-on={on || undefined}
        class="flex-1 hover:bg-neutral-500/10 data-[on]:bg-blue-500 data-[on]:hover:bg-blue-600"
    ></button>
{/snippet}
