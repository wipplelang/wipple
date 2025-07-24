<svelte:options
    customElement={{
        tag: "wipple-dropdown-widget",
        shadow: "none",
        props: {
            selection: { reflect: true },
        },
    }}
/>

<script lang="ts">
    import Icon from "$lib/components/Icon.svelte";
    import Menu from "$lib/components/Menu.svelte";
    import MenuButton from "$lib/components/MenuButton.svelte";
    import { trackWidgetValue } from "./util.svelte";

    interface Props {
        options: string[];
        selection: string;
    }

    let { options, selection = $bindable() }: Props = $props();

    trackWidgetValue($host(), () => ({ selection }));
</script>

<Menu class="inline-flex items-center">
    <button
        class="bg-background-secondary hover:bg-highlight-secondary flex h-full cursor-pointer items-center rounded-[8px] pl-[6px] transition-colors"
    >
        <code>{selection}</code>
        <Icon>keyboard_arrow_down</Icon>
    </button>

    {#snippet items()}
        {#each options as option}
            <MenuButton onclick={() => (selection = option)}>
                <code>{option}</code>
            </MenuButton>
        {/each}
    {/snippet}
</Menu>
