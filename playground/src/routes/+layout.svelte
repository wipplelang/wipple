<script lang="ts">
    import "$lib/styles/style.css";

    const { children } = $props();
</script>

<div class="not-printing:h-screen not-printing:w-screen flex flex-col">
    {@render children()}
</div>
