<script lang="ts">
    import Tooltip from "$lib/components/Tooltip.svelte";
    import {
        getDrumMachine,
        getSoundfontInstrument,
        initializeAudio,
        soundfontInstrumentNames,
        stopAllInstruments,
        type Instrument,
        getAudioContext,
    } from "$lib/assets/instruments";
    import Icon from "$lib/components/Icon.svelte";
    import { defaultTempo, type Music } from "$lib/assets/music";

    let isRunning = $state(false);
    let icon: HTMLDivElement;

    let together: number | undefined;
    let currentTempo: number;

    const waitForTogether = async () => {
        const duration = together;
        if (duration == null) {
            return;
        }

        await new Promise((resolve) => setTimeout(resolve, duration * 1000));

        together = undefined;
    };

    const runBeatAnimation = () => {
        icon.animate(
            { transform: ["scale(1)", "scale(1.1)", "scale(1)"] },
            { duration: 200, iterations: 1 },
        ).play();
    };

    export const _initializeOnClick = async () => {
        await initializeAudio();
    };

    export const _initialize = async () => {
        await Promise.all(Object.keys(soundfontInstrumentNames).map(getSoundfontInstrument));

        stopAllInstruments();

        together = undefined;
        currentTempo = defaultTempo;

        isRunning = true;
    };

    export const tempo = (tempo: number) => {
        currentTempo = tempo;
    };

    export const beginTogether = () => {
        together = 0;
    };

    export const endTogether = async () => {
        await waitForTogether();
    };

    export const play = async ([instrumentDropdownJson, musicJson]: [string, string]) => {
        const instrumentName = JSON.parse(instrumentDropdownJson).selection as string;
        const music = JSON.parse(musicJson).music as Music;

        let instrument: Instrument | undefined;
        switch (music.type) {
            case "melody": {
                if (instrumentName === "drums") {
                    console.warn("expected instrument, not drums");
                    return;
                }

                instrument = await getSoundfontInstrument(instrumentName);

                if (!instrument) {
                    throw new Error(`unknown instrument: ${instrumentName}`);
                }

                break;
            }
            case "rhythm": {
                if (instrumentName !== "drums") {
                    console.warn("expected drums, not instrument");
                    return;
                }

                instrument = await getDrumMachine();

                break;
            }
            default: {
                throw new Error(`unsupported music type: ${music.type}`);
            }
        }

        await instrument.init();

        const noteDuration = 60 / currentTempo;
        const totalDuration = noteDuration * music.notes.length;

        const startTime = getAudioContext().currentTime;
        let time = startTime;
        for (const notes of music.notes) {
            for (const note of notes) {
                instrument.play({ note, time, duration: noteDuration });
            }

            setTimeout(
                () => {
                    if (!isRunning) return;

                    runBeatAnimation();
                },
                (time - startTime) * 1000,
            );

            time += noteDuration;
        }

        if (together != null) {
            together = Math.max(together, totalDuration);
        } else {
            await new Promise((resolve) => setTimeout(resolve, totalDuration * 1000));
        }
    };

    export const _cleanup = async (force: boolean) => {
        if (!force) {
            await waitForTogether();

            // Let the last note fade out
            await new Promise((resolve) => setTimeout(resolve, (60 / defaultTempo) * 1000));
        }

        stopAllInstruments();

        isRunning = false;
    };
</script>

<div class="flex h-full flex-1">
    <div
        class={[
            "flex aspect-square flex-1 flex-row items-center justify-center gap-2 overflow-hidden rounded-lg border border-gray-100 p-2 transition dark:border-gray-800",
            isRunning ? "bg-orange-50 dark:bg-orange-950" : "",
        ]}
    >
        <div
            class={[
                "text-8xl transition",
                isRunning
                    ? "scale-150 text-orange-500"
                    : "scale-100 text-gray-300 dark:text-gray-600",
            ]}
        >
            <div bind:this={icon}>
                <Icon>music_note</Icon>
            </div>
        </div>
    </div>
</div>
