<script lang="ts">
    import { PUBLIC_VISUALIZER_URL } from "$env/static/public";
    import type * as wipple from "wipple";

    interface Props {
        graph: wipple.Graph | undefined;
        showFunctions?: boolean;
    }

    const { graph, showFunctions }: Props = $props();

    const visualizationUrl = (() => {
        const base = PUBLIC_VISUALIZER_URL;
        if (!base) {
            return undefined;
        }

        const url = new URL(base);

        url.searchParams.set("embed", "1");

        return url.toString();
    })();

    const sendEmbed = (visualizationWindow: Window) => {
        const embed = $state.snapshot(graph);
        if (embed == null) {
            return;
        }

        const show = {
            functions: showFunctions ?? false,
        };

        visualizationWindow.postMessage({ embed, show }, "*");
    };

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
                    sendEmbed(visualizationWindow);
                }
            });
        });
    });

    $effect(() => {
        graph;

        if (visualizationIFrame == null) {
            return;
        }

        const visualizationWindow = visualizationIFrame.contentWindow;
        if (!visualizationWindow) {
            return;
        }

        sendEmbed(visualizationWindow);
    });
</script>

<iframe
    bind:this={visualizationIFrame}
    title="Visualization"
    class="w-full h-full"
    src={visualizationUrl}
></iframe>
