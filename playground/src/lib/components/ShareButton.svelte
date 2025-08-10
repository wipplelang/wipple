<script lang="ts">
    import Icon from "./Icon.svelte";
    import ToolbarButton from "./ToolbarButton.svelte";
    import Tooltip from "./Tooltip.svelte";
    import * as shareApi from "$lib/share";
    import { context } from "$lib/context.svelte";

    const playground = $derived(context.playground);

    let loading = $state(false);
    let shareId = $state<string>();

    $effect(() => {
        playground;

        shareId = undefined;
    });

    const share = async () => {
        if (!playground || loading) return;

        loading = true;
        try {
            const { id } = await shareApi.share({
                runtime: playground.runtime,
                code: playground.code,
            });

            const shareUrl = `${window.location}?share=${id}`;
            navigator.clipboard.writeText(shareUrl);

            shareId = id;
            alert(`Share link copied to clipboard:\n\n${shareUrl}`);
        } catch (e) {
            console.error(e);
            alert("An error occurred while creating a link. Please try again.");
        } finally {
            loading = false;
        }
    };
</script>

<Tooltip content="Share" disabled={loading}>
    <ToolbarButton square={shareId == null} onclick={share} disabled={loading}>
        {#if loading}
            <Icon class="animate-spin">progress_activity</Icon>
        {:else}
            <Icon>{shareId ? "link" : "add_link"}</Icon>
            {shareId}
        {/if}
    </ToolbarButton>
</Tooltip>
