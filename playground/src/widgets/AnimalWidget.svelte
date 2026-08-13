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
    import AnimalPicker from "@/components/AnimalPicker.svelte";
    import AnimalPreview from "@/components/AnimalPreview.svelte";
    import Menu from "@/components/Menu.svelte";
    import ObjectWrapper from "@/components/ObjectWrapper.svelte";
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
