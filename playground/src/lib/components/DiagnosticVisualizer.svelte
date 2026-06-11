<script lang="ts">
    import type { Action } from "svelte/action";
    import Box from "./Box.svelte";
    import { fade, fly } from "svelte/transition";
    import ToolbarButton from "./ToolbarButton.svelte";
    import type { Diagnostic } from "wipple";
    import DiagnosticVisualizerNode from "./DiagnosticVisualizerNode.svelte";
    import DiagnosticVisualizerEdge from "./DiagnosticVisualizerEdge.svelte";
    import { Background, SvelteFlow, type Node, type Edge, MarkerType } from "@xyflow/svelte";
    import Dagre from "@dagrejs/dagre";

    interface Props {
        code: string;
        diagnostic: Diagnostic;
        onclose: () => void;
    }

    const { code, diagnostic, onclose }: Props = $props();

    const portal: Action = (node) => {
        $effect(() => {
            document.body.appendChild(node);

            return () => {
                node.remove();
            };
        });
    };

    const entries = [
        {
            id: "error",
            code,
            location: diagnostic.locations[0],
            message: diagnostic.message.split("\n\n")[0],
            consequences: [],
        },
        ...diagnostic.traces.map((trace) => ({
            id: `trace${trace.index}`,
            code,
            location: trace.location,
            message: trace.message,
            consequences: trace.consequences,
        })),
    ];

    let nodes = $state.raw(
        entries.map(
            (entry): Node => ({
                id: entry.id,
                type: "node",
                position: { x: Math.random() * 400, y: Math.random() * 400 },
                data: { ...entry },
                selectable: false,
                draggable: false,
            }),
        ),
    );

    const edges: Edge[] = [
        ...diagnostic.trace_edges.map(({ from, to }, index) => ({
            id: `edge${index}`,
            type: "edge",
            source: `trace${from}`,
            target: to != null ? `trace${to}` : "error",
            style: "stroke-width: 2px;",
            selectable: false,
            draggable: false,
            markerEnd: { type: MarkerType.ArrowClosed },
        })),
    ];

    const maxImportance = Math.max(...nodes.map((node) => node.data.importance as number));
    for (const node of nodes) {
        (node.data.importance as number) /= maxImportance;
    }

    let layouted = false;
    $effect(() => {
        const layoutNodes = $state.snapshot(nodes);

        if (layouted || layoutNodes.some((node) => node.measured == null)) {
            return;
        }

        const graph = new Dagre.graphlib.Graph().setDefaultEdgeLabel(() => ({}));
        graph.setGraph({ rankdir: "LR" });

        for (const node of layoutNodes) {
            console.log({ width: node.measured!.width, height: node.measured!.height });
            graph.setNode(node.id, {
                width: node.measured!.width!,
                height: node.measured!.height!,
            });
        }

        const nodeEdgeCounts = new Map<string, number>();
        for (const edge of edges) {
            graph.setEdge(edge.source, edge.target);

            for (const node of [edge.source, edge.target]) {
                nodeEdgeCounts.set(node, (nodeEdgeCounts.get(node) ?? 0) + 1);
            }
        }

        Dagre.layout(graph);

        for (const node of layoutNodes) {
            const { x, y } = graph.node(node.id);

            node.position = {
                x: x - node.measured!.width! / 2,
                y: y - node.measured!.height! / 2,
            };

            (node.data as any).importance = (nodeEdgeCounts.get(node.id) ?? 0) / edges.length;
        }

        nodes = layoutNodes as any;

        layouted = true;
    });
</script>

<div
    use:portal
    class="fixed inset-0 flex h-screen w-screen items-center justify-center bg-black/10 p-[20px]"
    transition:fade
>
    <div transition:fly={{ y: 50 }}>
        <Box
            class="flex w-screen max-w-[1200px] h-screen max-h-[800px] flex-col gap-[14px] p-[14px]"
        >
            <div
                class="flex h-(--toolbar-height) flex-1 flex-row gap-[10px] items-center justify-between"
            >
                <h2 class="text-lg font-semibold">Explore</h2>

                <ToolbarButton prominent onclick={onclose}>Done</ToolbarButton>
            </div>

            <div class="size-full overflow-scroll">
                <SvelteFlow
                    bind:nodes
                    {edges}
                    nodeTypes={{ node: DiagnosticVisualizerNode }}
                    edgeTypes={{ edge: DiagnosticVisualizerEdge }}
                    fitView
                    proOptions={{ hideAttribution: true }}
                >
                    <Background patternColor="var(--color-gray-300)" />
                </SvelteFlow>
            </div>
        </Box>
    </div>
</div>
