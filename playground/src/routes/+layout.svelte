<script lang="ts">
    import "$lib/styles/style.css";
    import * as Sentry from "@sentry/browser";
    import { PUBLIC_SENTRY_DSN } from "$env/static/public";

    const { children } = $props();

    Sentry.init({
        dsn: PUBLIC_SENTRY_DSN,
        enabled: import.meta.env.PROD,
    });
</script>

<div class="not-printing:h-screen not-printing:w-screen flex flex-col">
    {@render children()}
</div>
