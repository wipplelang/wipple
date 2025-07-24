<script lang="ts">
    import type { Command } from "$lib/models/Command";
    import type { Action } from "svelte/action";
    import CodeEditor from "./CodeEditor.svelte";
    import Documentation from "./Documentation.svelte";
    import Tooltip from "./Tooltip.svelte";

    const tooltipDelay = 750; // ms
    const stickyThresholdTime = 400; // ms
    const stickyThresholdDistance = 10; // px

    interface Props {
        tabindex: number;
        commandId: string;
        commandName: string;
        command: Command;
        dragInfo: { commandId: string; x: number; y: number; command: Command } | undefined;
        ondrop: () => void;
    }

    let {
        tabindex,
        commandId,
        commandName,
        command,
        dragInfo = $bindable(),
        ondrop,
    }: Props = $props();

    const isDragging = $derived(dragInfo?.commandId === commandId);

    const portal: Action = (node) => {
        $effect(() => {
            document.body.appendChild(node);

            return () => {
                node.remove();
            };
        });
    };

    const onmousedown = (e: MouseEvent) => {
        const startTimestamp = e.timeStamp;
        const startX = e.clientX;

        const update = (e: MouseEvent) => {
            dragInfo = { commandId, x: e.clientX, y: e.clientY, command };
        };

        const onmousemove = (e: MouseEvent) => {
            update(e);
        };

        const end = (drop: boolean) => {
            if (drop) {
                ondrop();
            }

            window.removeEventListener("mousemove", onmousemove);
            window.removeEventListener("mouseup", onmouseup);
            window.removeEventListener("keydown", onkeydown);
            dragInfo = undefined;
        };

        const onmouseup = (e: MouseEvent) => {
            if (
                e.timeStamp - startTimestamp > stickyThresholdTime ||
                Math.abs(e.clientX - startX) > stickyThresholdDistance
            ) {
                end(true);
            } else {
                // If the mouse is clicked instead of dragged, treat it as "sticky"
                setTimeout(() => {
                    window.addEventListener("click", () => end(true), { once: true });
                }, stickyThresholdTime);
            }
        };

        const onkeydown = (e: KeyboardEvent) => {
            if (e.key === "Escape") {
                end(false);
            }
        };

        window.addEventListener("mousemove", onmousemove);
        window.addEventListener("mouseup", onmouseup, { once: true });
        window.addEventListener("keydown", onkeydown);

        requestAnimationFrame(() => {
            update(e);
        });
    };
</script>

<Tooltip alignment="left" delay={tooltipDelay} defaultStyle={false} disabled={isDragging}>
    <div
        role="menuitem"
        {tabindex}
        class="hover-highlight -mx-[4px] cursor-pointer rounded-[8px] p-[2px]"
        style:visibility={isDragging ? "hidden" : "visible"}
        {onmousedown}
    >
        {#if isDragging}
            <div
                class="border-standard bg-background-secondary-alt shadow-standard pointer-events-none fixed size-max rounded-[8px] p-[4px]"
                style:top={dragInfo!.y + "px"}
                style:left={dragInfo!.x + "px"}
                use:portal
            >
                <CodeEditor readOnly code={command.code} />
            </div>
        {/if}

        <div class="pointer-events-none size-full">
            <CodeEditor readOnly code={commandName} />
        </div>
    </div>

    {#snippet content()}
        <Documentation name={commandName} {command} />
    {/snippet}
</Tooltip>
