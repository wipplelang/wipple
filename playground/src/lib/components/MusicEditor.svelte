<script lang="ts">
    import type { Action } from "svelte/action";
    import Box from "./Box.svelte";
    import { fade, fly } from "svelte/transition";
    import Tooltip from "./Tooltip.svelte";
    import ToolbarButton from "./ToolbarButton.svelte";
    import Icon from "./Icon.svelte";
    import MusicPreview from "./MusicPreview.svelte";
    import {
        initializeAudio,
        stopAllInstruments,
        type Instrument,
        getAudioContext,
    } from "$lib/assets/instruments";
    import randomColor from "randomcolor";
    import MenuButton from "./MenuButton.svelte";
    import Menu from "./Menu.svelte";
    import { defaultTempo, maxNotes, notesInMeasure, type Music } from "$lib/assets/music";
    import PianoRoll from "./PianoRoll.svelte";

    interface Props {
        icon: string;
        music: Music;
        instrument: Instrument;
        ondismiss: () => void;
    }

    let { icon, music = $bindable(), instrument, ondismiss }: Props = $props();

    let musicDraft = $state(music);
    let playing = $state<AbortController>();

    const portal: Action = (node) => {
        $effect(() => {
            document.body.appendChild(node);

            return () => {
                node.remove();
            };
        });
    };

    const onclick = async () => {
        await initializeAudio();
    };

    $effect(() => {
        window.addEventListener("click", onclick, { once: true });
    });

    const onplay = async () => {
        if (playing) {
            stopAllInstruments();
            playing.abort();
            playing = undefined;
            return;
        }

        playing = new AbortController();

        await instrument.init();

        const noteDuration = 60 / (defaultTempo * notesInMeasure);

        let time = getAudioContext().currentTime;
        for (const notes of musicDraft.notes) {
            for (const note of notes) {
                instrument.play({ note, time, duration: noteDuration });
            }

            time += noteDuration;
        }

        const totalDuration = noteDuration * musicDraft.notes.length * 1000;
        await new Promise<void>((resolve) => {
            playing!.signal.addEventListener("abort", () => resolve());
            setTimeout(resolve, totalDuration);
        });

        playing = undefined;
    };

    const clear = () => {
        if (!confirm("Are you sure you want to clear?")) {
            return;
        }

        for (let i = 0; i < musicDraft.notes.length; i++) {
            musicDraft.notes[i] = [];
        }
    };

    const setNumberOfNotes = (count: number) => {
        if (count < musicDraft.notes.length) {
            musicDraft.notes = musicDraft.notes.slice(0, count);
        } else {
            for (let i = musicDraft.notes.length; i < count; i++) {
                musicDraft.notes.push([]);
            }
        }
    };

    const onplaynote = async (note: string) => {
        await instrument.init();
        instrument.play({ note });
    };

    const onsubmit = () => {
        music = musicDraft;
        ondismiss();
    };
</script>

<div
    use:portal
    class="fixed inset-0 flex h-screen w-screen items-center justify-center bg-black/10 p-[20px]"
    transition:fade
>
    <div transition:fly={{ y: 50 }}>
        <Box class="flex w-[650px] flex-col gap-[14px] p-[14px]">
            <div class="flex h-(--toolbar-height) flex-1 flex-row gap-[10px]">
                <Tooltip content="Change color" onclick={() => (musicDraft.color = randomColor())}>
                    <Box class="size-(--toolbar-height) text-xl">
                        <MusicPreview {icon} color={musicDraft.color} />
                    </Box>
                </Tooltip>

                <ToolbarButton
                    prominent
                    onclick={onplay}
                    data-playing={playing || undefined}
                    class="data-[prominent]:data-[playing]:bg-sky-500"
                >
                    {#if playing}
                        <Icon fill>stop</Icon>
                        Stop
                    {:else}
                        <Icon fill>play_arrow</Icon>
                        Play
                    {/if}
                </ToolbarButton>

                <ToolbarButton onclick={clear}>Clear</ToolbarButton>

                <div class="border-standard h-(--toolbar-height) w-0 rounded-full"></div>

                <Menu>
                    <ToolbarButton class="pr-[6px] pl-[10px]">
                        {musicDraft.notes.length}
                        {musicDraft.notes.length === 1 ? "note" : "notes"}
                        <Icon>keyboard_arrow_down</Icon>
                    </ToolbarButton>

                    {#snippet items()}
                        {#each new Array(maxNotes).fill(undefined) as _, index}
                            <MenuButton onclick={() => setNumberOfNotes(index + 1)}>
                                {index + 1}
                                {index === 0 ? "note" : "notes"}
                            </MenuButton>
                        {/each}
                    {/snippet}
                </Menu>

                <div class="flex-1"></div>

                <ToolbarButton prominent onclick={onsubmit}>Done</ToolbarButton>
            </div>

            <PianoRoll bind:music={musicDraft} {onplaynote} />
        </Box>
    </div>
</div>
