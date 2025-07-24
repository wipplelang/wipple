<script lang="ts">
    import type { Snippet } from "svelte";
    import type { HTMLAttributes, MouseEventHandler } from "svelte/elements";
    import { scale } from "svelte/transition";
    import type { Action } from "svelte/action";

    interface Props extends HTMLAttributes<HTMLElement> {
        children: Snippet;
        items: Snippet;
    }

    const duration = 150;
    const topOffset = 4;

    const { children, items, ...props }: Props = $props();

    const portal: Action = (node) => {
        $effect(() => {
            document.body.appendChild(node);

            return () => {
                node.remove();
            };
        });
    };

    let element: HTMLElement;
    let wrapper = $state<HTMLElement>();
    let wrapperVisible = $state(false);
    let contentVisible = $state(false);

    const onclick: MouseEventHandler<HTMLElement> = (e) => {
        const reference = e.currentTarget;

        if (wrapperVisible) {
            dismiss();
            return;
        }

        wrapperVisible = true;

        requestAnimationFrame(() => {
            const referenceRect = reference.getBoundingClientRect();

            const x = referenceRect.left;
            const y = referenceRect.top + referenceRect.height + topOffset;

            wrapper!.style.position = "fixed";
            wrapper!.style.left = `${x}px`;
            wrapper!.style.top = `${y}px`;
            contentVisible = true;
        });
    };

    const dismiss = () => {
        contentVisible = false;

        setTimeout(() => {
            wrapperVisible = false;
        }, duration);
    };

    const onClickOutside: MouseEventHandler<Window> = (e) => {
        if (!element.contains(e.target as Node)) {
            dismiss();
        }
    };
</script>

<svelte:window onclick={onClickOutside} />

<!-- Don't add whitespace between the snippet and the menu div -->
<span bind:this={element} {onclick} {...props}>
    {@render children()}{#if wrapperVisible}
        <div bind:this={wrapper} use:portal class="fixed" role="menu">
            {#if contentVisible}
                <!-- svelte-ignore a11y_click_events_have_key_events -->
                <!-- svelte-ignore a11y_no_static_element_interactions -->
                <div
                    transition:scale={{ duration, start: 0.95 }}
                    onclick={dismiss}
                    class="border-standard bg-overlay shadow-standard flex min-w-[150px] flex-col items-stretch justify-start gap-[4px] overflow-scroll rounded-[10px] px-[4px] py-[4px] text-sm"
                >
                    {@render items()}
                </div>
            {/if}
        </div>
    {/if}
</span>
