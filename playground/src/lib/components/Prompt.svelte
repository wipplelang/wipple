<script lang="ts">
    import Icon from "./Icon.svelte";

    interface Props {
        prompt: string;
        onsubmit: (value: string) => Promise<boolean>;
    }

    const { prompt, onsubmit }: Props = $props();

    let value = $state("");
    let valid = $state(true);
    let disabled = $state(false);
    let submitted = $state(false);
    let input: HTMLInputElement;

    $effect(() => {
        value;
        valid = true;
    });

    const submit = async () => {
        valid = true;
        disabled = true;

        submitted = await onsubmit(value);

        if (submitted) {
            valid = true;
            disabled = true;
        } else {
            valid = false;
            disabled = false;

            requestAnimationFrame(() => {
                input.focus();
            });
        }
    };
</script>

<form
    onsubmit={(e) => {
        e.preventDefault();
        submit();
    }}
    class={["flex flex-row gap-[10px]", valid ? "" : "animate-[validate-shake_500ms]"]}
>
    <input
        bind:this={input}
        {disabled}
        placeholder={prompt}
        onchange={(e) => (value = e.currentTarget.value)}
        class={[
            "-mx-[4px] flex-1 rounded-[4px] px-[4px] py-[2px]",
            valid ? "focus:outline-blue-500" : "bg-red-500/10 focus:outline-red-500",
        ]}
    />

    <button
        type="submit"
        {disabled}
        class="bg-background-button my-auto flex h-6 w-6 cursor-pointer items-center justify-center rounded-md text-white disabled:bg-gray-300 disabled:dark:bg-gray-800"
    >
        <Icon>arrow_forward</Icon>
    </button>
</form>
