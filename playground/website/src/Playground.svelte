<script lang="ts">
    import SplitEditors, {
        grayOutputColor,
        redOutputColor,
    } from "./SplitEditors.svelte";

    export let interpreter;

    const query = new URLSearchParams(window.location.search);

    let code = query.get("code") || "-- Write your code here!\n\n";
    let output = "";
    let outputColor = grayOutputColor;

    const debounce = (callback, wait, immediate = false) => {
        let timeout = null;

        return () => {
            const callNow = immediate && !timeout;
            const next = () => callback.apply(this, arguments);

            clearTimeout(timeout);
            timeout = setTimeout(next, wait());

            if (callNow) {
                next();
            }
        };
    };

    const handleChange = debounce(
        () => {
            query.set("code", code);
            const newURL = window.location.pathname + "?" + query.toString();
            window.history.replaceState(null, "", newURL);

            handleLoading();
            const result = interpreter.run(code);
            handleResult(result);
        },
        // Adjust the delay based on the size of the code (lines is a rough
        // but OK metric)
        () => Math.min(code.split("\n").length * 20, 1000)
    );

    const handleLoading = () => {
        output = "Running...";
        outputColor = grayOutputColor;
    };

    const handleResult = (result) => {
        if (result.success) {
            if (result.output.length === 0) {
                output = "No output\nTip: Use 'show' to display values here";
                outputColor = grayOutputColor;
            } else {
                output = result.output.map((entry) => entry.output).join("\n");
                outputColor = undefined;
            }
        } else {
            output = result.error;
            outputColor = redOutputColor;
        }
    };
</script>

<SplitEditors bind:code bind:output {outputColor} change={handleChange} />
