import { PaletteItem } from "../../models";

export const Palette = (props: { items: PaletteItem[] }) => (
    <div className="flex flex-row items-center px-1 overflow-x-scroll">
        {props.items.map((item) => (
            <div
                key={item.title}
                draggable
                onDragStart={(event) => {
                    event.dataTransfer.setData("wipple/snippet", item.code);
                }}
            >
                <div className="flex items-center mb-[1px] px-1 hover:scale-105 transition-transform">
                    <code className="h-4 text-xs whitespace-nowrap text-gray-900 dark:text-white">
                        {item.title}
                    </code>
                </div>
            </div>
        ))}
    </div>
);
