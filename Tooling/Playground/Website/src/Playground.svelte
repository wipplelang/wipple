<script lang="ts">
    import SplitEditors, {
        grayOutputColor,
        redOutputColor,
    } from "./SplitEditors.svelte";

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
        async () => {
            query.set("code", code[0].text);
            const newURL = window.location.pathname + "?" + query.toString();
            window.history.replaceState(null, "", newURL);

            handleLoading();
            const result = await run(code[0].text);
            handleResult(result);
        },
        () => 1500
    );

    const run = async (code: string) => {
        const url = `${window.origin}/.netlify/functions/run`;

        const response = await fetch(url, {
            method: "POST",
            body: code,
        });

        return await response.json();
    };

    const handleLoading = () => {
        output = [{ text: "Running...", color: grayOutputColor }];
    };

    const handleResult = (result) => {
        output =
            result.length === 0
                ? [
                      {
                          text: "No output\nYou can display values here using 'show'",
                          color: grayOutputColor,
                      },
                  ]
                : result.map((item) => ({
                      text: item.text,
                      color: item.success ? undefined : redOutputColor,
                  }));
    };
</script>

<SplitEditors bind:code bind:output change={handleChange} />
