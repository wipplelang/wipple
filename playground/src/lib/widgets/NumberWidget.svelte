<svelte:options
    customElement={{
        tag: "wipple-number-widget",
        shadow: "none",
        props: {
            number: { reflect: true },
        },
    }}
/>

<script lang="ts">
    import Icon from "$lib/components/Icon.svelte";
    import Menu from "$lib/components/Menu.svelte";
    import MenuButton from "$lib/components/MenuButton.svelte";
    import type { UnitInfo } from "$lib/models/Runtime";
    import { trackWidgetValue } from "./util.svelte";

    interface Props {
        number: number;
        unit: string;
        unitInfo: UnitInfo;
    }

    let { number = $bindable(), unit, unitInfo }: Props = $props();
    const { presets } = unitInfo;

    trackWidgetValue($host(), () => ({ number }));

    const customNumber = () => {
        const input = prompt(`Number of ${unit}:`, number.toString());
        if (input == null) {
            return;
        }

        number = parseFloat(input);
    };
</script>

<Menu class="inline-flex items-center">
    <button
        class="bg-background-secondary hover:bg-highlight-secondary ml-[4px] flex cursor-pointer items-center rounded-[4px] text-xs transition-colors"
    >
        <Icon>unfold_more</Icon>
    </button>

    {#snippet items()}
        {#each presets as preset}
            <MenuButton onclick={() => (number = preset)}>
                <code>{preset} <span class="opacity-50">{unit}</span></code>
            </MenuButton>
        {/each}

        <MenuButton onclick={customNumber}>Custom...</MenuButton>
    {/snippet}
</Menu>
