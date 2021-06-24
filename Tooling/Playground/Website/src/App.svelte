<script lang="ts">
    import Header from "./Header.svelte";
    import Playground from "./Playground.svelte";
</script>

<div class="flex flex-col space-y-6 p-6">
    <Header />
    <Playground />
</div>

<style global lang="postcss">
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
</style>
