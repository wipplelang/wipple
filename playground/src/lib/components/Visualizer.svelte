<script lang="ts">
    import { PUBLIC_VISUALIZER_URL } from "$env/static/public";

    interface Props {
        graph: any;
        showFunctionsAndStatements?: boolean;
    }

    const { graph, showFunctionsAndStatements }: Props = $props();

    const visualizationUrl = $derived.by(() => {
        if (!graph) {
            return undefined;
        }

        const base = PUBLIC_VISUALIZER_URL;
        if (!base) {
            return undefined;
        }

        const url = new URL(base);

        url.searchParams.set("embed", "1");

        if (showFunctionsAndStatements) {
            url.searchParams.set("showFunctionsAndStatements", "1");
        }

        return url.toString();
    });

    let visualizationIFrame = $state<HTMLIFrameElement>();
    $effect(() => {
        if (visualizationIFrame == null) {
            return;
        }

        visualizationIFrame.addEventListener("load", (event) => {
            const visualizationWindow = (event.target as HTMLIFrameElement).contentWindow!;

            window.addEventListener("message", (event) => {
                if (event.source !== visualizationWindow) {
                    return;
                }

                if (event.data === "requestEmbed") {
                    visualizationWindow.postMessage({ embed: $state.snapshot(graph) }, "*");
                }
            });
        });
    });
</script>

{#key graph}
    <iframe
        bind:this={visualizationIFrame}
        title="Visualization"
        class="aspect-[3/2] flex-1 rounded-xl border-[1.5px] border-black/5 dark:bg-gray-800"
        src={visualizationUrl}
    ></iframe>
{/key}
