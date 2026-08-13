<svelte:options
    customElement={{
        tag: "wipple-color-widget",
        shadow: "none",
        props: {
            color: { reflect: true },
        },
    }}
/>

<script lang="ts">
    import colors from "@/assets/colors";
    import ColorPalette from "@/components/ColorPalette.svelte";
    import ColorPreview from "@/components/ColorPreview.svelte";
    import Menu from "@/components/Menu.svelte";
    import ObjectWrapper from "@/components/ObjectWrapper.svelte";
    import { trackWidgetValue } from "./util.svelte";

    interface Props {
        color: string;
    }

    let { color = $bindable() }: Props = $props();

    trackWidgetValue($host(), () => ({ color }));

    const { varName } = $derived(
        color in colors ? colors[color as keyof typeof colors] : colors["gray 2"],
    );
</script>

<Menu class="inline-flex">
    <ObjectWrapper>
        <ColorPreview name={color} {varName} />
    </ObjectWrapper>

    {#snippet items()}
        <ColorPalette bind:color />
    {/snippet}
</Menu>
