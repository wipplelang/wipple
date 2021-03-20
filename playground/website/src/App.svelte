<script lang="ts">
    import Header from "./Header.svelte";
    import Playground from "./Playground.svelte";

    export let interpreter;
</script>

<div class="flex flex-col space-y-6 p-6">
    <Header />
    <Playground {interpreter} />
</div>

<style global lang="postcss">
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
