<svelte:options
    customElement={{
        tag: "wipple-music-widget",
        shadow: "none",
        props: {
            value: { reflect: true },
        },
    }}
/>

<script lang="ts">
    import MusicPreview from "$lib/components/MusicPreview.svelte";
    import ObjectWrapper from "$lib/components/ObjectWrapper.svelte";
    import { trackWidgetValue } from "./util.svelte";
    import { type Music } from "$lib/assets/music";
    import MusicEditor from "$lib/components/MusicEditor.svelte";
    import { getDefaultSoundfontInstrument, getDrumMachine } from "$lib/assets/instruments";

    interface Props {
        music: Music;
    }

    let { music = $bindable() }: Props = $props();

    trackWidgetValue($host(), () => ({ music }));

    let editorVisible = $state(false);

    const icon = music.type === "melody" ? "piano" : "music_note";

    const getInstrument = () =>
        music.type === "melody" ? getDefaultSoundfontInstrument() : getDrumMachine();
</script>

<ObjectWrapper onclick={() => (editorVisible = true)}>
    <MusicPreview {icon} color={music.color} />

    {#if editorVisible}
        {#await getInstrument() then instrument}
            <MusicEditor {icon} bind:music {instrument} ondismiss={() => (editorVisible = false)} />
        {/await}
    {/if}
</ObjectWrapper>
