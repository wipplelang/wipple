<script lang="ts">
    import { type Snippet } from "svelte";
    import type { HTMLAttributes, MouseEventHandler } from "svelte/elements";
    import type { Action } from "svelte/action";

    interface Props extends HTMLAttributes<HTMLElement> {
        children?: Snippet;
        alignment?: "left" | "center";
        delay?: number;
        defaultStyle?: boolean;
        content?: string | Snippet;
        disabled?: boolean;
    }

    const duration = 150;
    const topOffset = 4;

    const {
        children,
        alignment = "center",
        delay = 150,
        content,
        defaultStyle = true,
        disabled = false,
        ...props
    }: Props = $props();

    const id = $props.id();

    const portal: Action = (node) => {
        $effect(() => {
            document.body.appendChild(node);

            return () => {
                node.remove();
            };
        });
    };

    let wrapper = $state<HTMLElement>();
    let wrapperVisible = $state(false);
    let contentVisible = $state(false);

    let timeout: number | undefined;
    const onmouseenter: MouseEventHandler<HTMLElement> = (e) => {
        if (disabled) {
            return;
        }

        const reference = e.currentTarget;

        timeout = setTimeout(() => {
            timeout = undefined;

            wrapperVisible = true;

            requestAnimationFrame(() => {
                const referenceRect = reference.getBoundingClientRect();
                const wrapperRect = wrapper!.getBoundingClientRect();

                const x =
                    alignment === "left"
                        ? referenceRect.left
                        : referenceRect.left + referenceRect.width / 2 - wrapperRect.width / 2;

                const y = referenceRect.top + referenceRect.height + topOffset;

                wrapper!.style.position = "fixed";
                wrapper!.style.left = `${x}px`;
                wrapper!.style.top = `${y}px`;
                contentVisible = true;
            });
        }, delay);
    };

    const dismiss = () => {
        clearTimeout(timeout);
        timeout = undefined;

        contentVisible = false;

        setTimeout(() => {
            wrapperVisible = false;
        }, duration);
    };
</script>

<div
    {onmouseenter}
    onmouseleave={dismiss}
    onmousedown={dismiss}
    aria-describedby={`${id}-tooltip`}
    {...props}
>
    <!-- Don't add whitespace between the snippet and the tooltip div -->
    {@render children?.()}{#if wrapperVisible}
        <div
            bind:this={wrapper}
            use:portal
            id={`${id}-tooltip`}
            data-visible={(content != null && contentVisible) || undefined}
            class={[
                "inline-block", // required to properly compute width
                "border-standard bg-overlay text-secondary shadow-standard pointer-events-none rounded-[16px]",
                defaultStyle && "px-[10px] py-[4px] text-sm",
                "scale-95 opacity-0 transition-all data-[visible]:scale-100 data-[visible]:opacity-100",
            ]}
        >
            {#if typeof content === "string"}
                {content}
            {:else}
                {@render content?.()}
            {/if}
        </div>
    {/if}
</div>
