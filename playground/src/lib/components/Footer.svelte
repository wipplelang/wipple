<script lang="ts">
    import * as Sentry from "@sentry/browser";

    const onreportissue = (e: MouseEvent) => {
        Sentry.getFeedback()?.attachTo(e.target as HTMLElement, {
            formTitle: "Report Issue",
            showName: false,
            showEmail: false,
            showBranding: false,
            submitButtonLabel: "Submit",
        });
    };
</script>

{#snippet separator()}
    <span class="opacity-25">•</span>
{/snippet}

{#snippet link(url: string, title: string)}
    <a href={url} target="_blank" class="opacity-60 hover:opacity-100">{title}</a>
{/snippet}

<footer
    class="printing:hidden flex flex-row items-center justify-between px-[20px] pb-[10px] text-sm"
>
    <div class="flex flex-row items-center gap-[8px]">
        {@render link("/", "About")}
        {@render separator()}
        {@render link("/docs", "Guide")}
        {@render separator()}
        {@render link("https://github.com/wipplelang/wipple/blob/main/CHANGELOG.md", "What’s New")}
    </div>

    <div class="flex flex-row items-center gap-[8px]">
        <button onclick={onreportissue} class="cursor-pointer opacity-60 hover:opacity-100">
            Report Issue
        </button>
        {@render separator()}
        {@render link("https://gramer.dev", "Made by Wilson Gramer")}
    </div>
</footer>
