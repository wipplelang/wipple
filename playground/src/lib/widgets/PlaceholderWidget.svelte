<svelte:options
    customElement={{
        tag: "wipple-placeholder-widget",
        shadow: "none",
    }}
/>

<script lang="ts">
    import Icon from "$lib/components/Icon.svelte";
    import ObjectWrapper from "$lib/components/ObjectWrapper.svelte";

    interface Props {}

    const {}: Props = $props();
</script>

<ObjectWrapper class="flex items-center justify-center text-xs" scaleOnHover={false}>
    <Icon class="text-white">question_mark</Icon>
</ObjectWrapper>
