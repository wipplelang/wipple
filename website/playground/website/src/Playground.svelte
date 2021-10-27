<script lang="ts">
    import SplitEditors, {
        grayOutputColor,
        redOutputColor,
    } from "./SplitEditors.svelte";

    export let runner;

    const query = new URLSearchParams(window.location.search);

    let code = [
        {
            text: query.get("code") || "-- Write your code here!\n\n",
            color: grayOutputColor,
        },
    ];

    let output = [];

    const debounce = (callback, wait) => {
        let timeout = null;

        return () => {
            const next = () => callback.apply(this, arguments);

            clearTimeout(timeout);
            timeout = setTimeout(next, wait());
        };
    };

    const handleChange = debounce(
        () => {
            query.set("code", code[0].text);
            const newURL = window.location.pathname + "?" + query.toString();
            window.history.replaceState(null, "", newURL);

            handleLoading();
            const result = runner.run(code[0].text);
            handleResult(result);
        },
        // Adjust the delay based on the size of the code (lines is a rough
        // but OK metric)
        () => Math.min(code[0].text.split("\n").length * 20, 1000)
    );

    const handleLoading = () => {
        output = [{ text: "Running...", color: grayOutputColor }];
    };

    const handleResult = (result) => {
        // output =
        //     result.length === 0
        //         ? [
        //               {
        //                   text: "No output\nYou can display values here using 'show'",
        //                   color: grayOutputColor,
        //               },
        //           ]
        //         : result.map((item) => ({
        //               text: item.text,
        //               color: item.success ? undefined : redOutputColor,
        //           }));

        output = [
            {
                text: result,
                color: undefined,
            },
        ];
    };
</script>

<SplitEditors bind:code bind:output change={handleChange} />
