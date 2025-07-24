<script lang="ts">
    import colors from "$lib/assets/colors";
    import Icon from "./Icon.svelte";

    interface Props {
        color?: string;
        icon: string;
    }

    const { color, icon }: Props = $props();
</script>

<div
    class="flex aspect-square flex-1 cursor-pointer select-none items-center justify-center text-[smaller] text-white"
    style="background-color: {color ?? colors['gray 2']}"
>
    <Icon>{icon}</Icon>
</div>
