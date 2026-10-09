<script lang="ts">
    import Icon from "./Icon.svelte";
    import ToolbarButton from "./ToolbarButton.svelte";
    import Tooltip from "./Tooltip.svelte";

    const print = () => {
        window.print();
    };
</script>

<Tooltip content="Print">
    <ToolbarButton square onclick={print}>
        <Icon>print</Icon>
    </ToolbarButton>
</Tooltip>
