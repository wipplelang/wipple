<script lang="ts">
    import "@/styles/style.css";
    import * as Sentry from "@sentry/browser";
    import App from "./App.svelte";

    Sentry.init({
        dsn: import.meta.env.VITE_SENTRY_DSN,
        enabled: import.meta.env.PROD,
        integrations: [
            Sentry.feedbackIntegration({
                colorScheme: "system",
                autoInject: false,
            }),
        ],
    });
</script>

<div class="not-printing:h-screen not-printing:w-screen flex flex-col">
    <App />
</div>
