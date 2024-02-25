import { Asset } from "./asset";

export const Palette = (props: { onClose: () => void }) => {
    const assets = ["color:#ef4444", "color:#22c55e", "color:#3b82f6"];

    return (
        <div className="flex flex-row items-center gap-0.5">
            {assets.map((asset) => (
                <div
                    key={asset}
                    draggable
                    onDragStart={(event) => {
                        event.dataTransfer.setData("wipple/asset", asset);
                    }}
                    onDragLeave={props.onClose}
                >
                    <Asset>{asset}</Asset>
                </div>
            ))}
        </div>
    );
};
