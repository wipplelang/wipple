<script lang="ts">
    import Icon from "./Icon.svelte";
    import ToolbarButton from "./ToolbarButton.svelte";
    import Tooltip from "./Tooltip.svelte";
    import { jsPDF } from "jspdf";
    import html2canvas from "html2canvas-pro";

    const print = async () => {
        const scale = 4;
        const margin = 0.5;
        const width = 11 - margin * 2;
        const height = 8.5 - margin * 2;
        const filename = `wipple-${new Date().toISOString().replace(/[^0-9]/g, "")}.pdf`;

        const canvas = await html2canvas(document.getElementById("root")!, {
            scale,
            width: width * 100,
            height: height * 100,
            useCORS: true,
            onclone: (_document, element) => {
                element.style.width = `${width * 100}px`;
                element.style.height = `${height * 100}px`;
                element.dataset.printing = "printing";
            },
            ignoreElements: (element) => element.classList.contains("cm-widgetBuffer"),
        });

        const pdf = new jsPDF({
            unit: "in",
            format: "letter",
            orientation: "landscape",
            compress: true,
        });

        pdf.addImage({
            imageData: canvas.toDataURL("image/png"),
            format: "PNG",
            x: margin,
            y: margin,
            width,
            height,
        });

        pdf.save(filename);
    };
</script>

<Tooltip content="Print">
    <ToolbarButton square onclick={print}>
        <Icon>print</Icon>
    </ToolbarButton>
</Tooltip>
