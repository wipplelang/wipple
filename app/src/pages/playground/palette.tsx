import { MaterialSymbol } from "react-material-symbols";
import { Asset, isAsset } from "./asset";

const assets = [`{color "#ef4444"}`, `{color "#22c55e"}`, `{color "#3b82f6"}`];
const snippets = [`show _`, `repeat (1 times) (\n  _\n)`];

export const AssetPalette = (props: { onClose: () => void }) => (
    <Palette items={assets} onClose={props.onClose} />
);

export const SnippetPalette = (props: { onClose: () => void }) => (
    <Palette items={snippets} showSeparator onClose={props.onClose} />
);

const Palette = (props: { items: string[]; showSeparator?: boolean; onClose: () => void }) => (
    <div className="flex flex-row items-center gap-0.5">
        {props.items.map((item, index) => (
            <>
                {props.showSeparator && index > 0 ? (
                    <div className="rounded-full mx-0.5 border-l-2 border-gray-100 dark:border-gray-800 h-[18px]" />
                ) : null}

                <div
                    key={item}
                    draggable
                    onDragStart={(event) => {
                        event.dataTransfer.setData("wipple/snippet", item);
                    }}
                    onDragLeave={props.onClose}
                >
                    {isAsset(item) ? (
                        <Asset>{item.slice(1, item.length - 1)}</Asset>
                    ) : (
                        <div className="flex items-center mb-[1px] px-0.5 hover:scale-110 transition-transform">
                            <code className="h-4 text-xs whitespace-nowrap text-black dark:text-white">
                                {item.split(" ")[0]}
                            </code>
                        </div>
                    )}
                </div>
            </>
        ))}

        <button
            className="flex items-center w-4 h-4 ml-0.5 mb-[1px] rounded-full bg-gray-100 dark:bg-gray-800 hover:bg-gray-200 hover:dark:bg-gray-700 transition-colors"
            onClick={props.onClose}
        >
            <MaterialSymbol icon="close_small" />
        </button>
    </div>
);
