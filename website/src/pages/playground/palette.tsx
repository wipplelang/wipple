import { Asset, isAsset } from "./asset";

export const Palette = (props: { onClose: () => void }) => {
    const snippets = [`{color "#ef4444"}`, `{color "#22c55e"}`, `{color "#3b82f6"}`];

    return (
        <div className="flex flex-row items-center gap-0.5">
            {snippets.map((snippet) => (
                <div
                    key={snippet}
                    draggable
                    onDragStart={(event) => {
                        event.dataTransfer.setData("wipple/snippet", snippet);
                    }}
                    onDragLeave={props.onClose}
                >
                    {isAsset(snippet) ? (
                        <Asset>{snippet.slice(1, snippet.length - 1)}</Asset>
                    ) : (
                        <div>{snippet}</div>
                    )}
                </div>
            ))}
        </div>
    );
};
