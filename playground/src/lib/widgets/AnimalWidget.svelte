<svelte:options
    customElement={{
        tag: "wipple-animal-widget",
        shadow: "none",
        props: {
            animal: { reflect: true },
        },
    }}
/>

<script lang="ts">
    import AnimalPicker from "$lib/components/AnimalPicker.svelte";
    import AnimalPreview from "$lib/components/AnimalPreview.svelte";
    import Menu from "$lib/components/Menu.svelte";
    import ObjectWrapper from "$lib/components/ObjectWrapper.svelte";
    import { trackWidgetValue } from "./util.svelte";

    interface Props {
        animal: string;
    }

    let { animal = $bindable() }: Props = $props();

    trackWidgetValue($host(), () => ({ animal }));
</script>

<Menu class="inline-flex">
    <ObjectWrapper>
        <AnimalPreview name={animal} />
    </ObjectWrapper>

    {#snippet items()}
        <AnimalPicker onclick={(a) => (animal = a)} />
    {/snippet}
</Menu>
