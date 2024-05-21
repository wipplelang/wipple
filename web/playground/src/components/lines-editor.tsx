import html2canvas from "html2canvas";
import { useCallback, useEffect, useRef, useState } from "react";
import { MaterialSymbol } from "react-material-symbols";
import { SortableContainer, SortableElement, SortableHandle } from "react-sortable-hoc";

export const LinesEditor = (
    props: React.PropsWithChildren<{
        numberOfLines: number;
        lineHeight: number;
        onReorderLine: (from: number, to: number) => void;
    }>,
) => {
    const childrenRef = useRef<HTMLDivElement>(null);

    const [isDragging, setDragging] = useState(false);
    const [linesImages, setLinesImages] = useState<string[]>([]);

    const updateLinesImages = useCallback(async () => {
        const scaledLineHeight = props.lineHeight * window.devicePixelRatio;

        const canvas = await html2canvas(childrenRef.current!, {
            backgroundColor: "transparent",
            logging: false,
        });

        const linesImages: string[] = [];
        for (let y = 0; y < canvas.height; y += scaledLineHeight) {
            const lineCanvas = document.createElement("canvas");

            lineCanvas.width = canvas.width;
            lineCanvas.height = scaledLineHeight;

            const ctx = lineCanvas.getContext("2d")!;
            ctx.drawImage(
                canvas,
                0,
                y,
                lineCanvas.width,
                scaledLineHeight,
                0,
                0,
                lineCanvas.width,
                lineCanvas.height,
            );

            linesImages.push(lineCanvas.toDataURL());
        }

        setLinesImages(linesImages);
    }, [props.numberOfLines]);

    useEffect(() => {
        updateLinesImages();
    }, [updateLinesImages]);

    return (
        <div className="relative">
            <div className="absolute inset-0">
                <DraggableLinesContainer
                    useDragHandle
                    helperClass="pointer-events-none"
                    lockAxis="y"
                    pressDelay={200}
                    onSortEnd={({ oldIndex, newIndex }) => {
                        setDragging(false);
                        props.onReorderLine(oldIndex, newIndex);
                    }}
                >
                    {linesImages.map((lineImage, index) => (
                        <DraggableLinesItem
                            key={index}
                            index={index}
                            height={props.lineHeight}
                            onSelect={async () => {
                                await updateLinesImages();
                                setDragging(true);
                            }}
                        >
                            <div className="select-none pointer-events-none">
                                {isDragging ? <img src={lineImage} /> : null}
                            </div>
                        </DraggableLinesItem>
                    ))}
                </DraggableLinesContainer>
            </div>

            <div className={`ml-4 ${isDragging ? "opacity-0 pointer-events-none" : ""}`}>
                <div ref={childrenRef}>{props.children}</div>
            </div>
        </div>
    );
};

interface DragHandleProps {
    onSelect: () => void;
}

const DragHandle = SortableHandle<DragHandleProps>((props: DragHandleProps) => {
    const [isSelected, setSelected] = useState(false);

    return (
        <div
            className="select-none z-10"
            onMouseDown={() => {
                setSelected(true);
                props.onSelect();
            }}
            onMouseLeave={() => {
                setSelected(false);
            }}
        >
            <div
                className={`w-4 h-full text-gray-400 dark:text-gray-600 opacity-0 hover:opacity-100 ${
                    isSelected ? "opacity-100" : ""
                } transition-opacity`}
            >
                <MaterialSymbol icon="drag_indicator" className="align-middle" />
            </div>
        </div>
    );
});

type DraggableLinesContainerProps = React.PropsWithChildren<{}>;

const DraggableLinesContainer = SortableContainer<DraggableLinesContainerProps>(
    (props: React.PropsWithChildren<{}>) => <div className="flex flex-col">{props.children}</div>,
);

type DraggableLinesItemProps = React.PropsWithChildren<{
    height: number;
    onSelect: () => void;
}>;

const DraggableLinesItem = SortableElement<DraggableLinesItemProps>(
    (props: DraggableLinesItemProps) => (
        <div className="flex flex-row" style={{ height: props.height }}>
            <DragHandle onSelect={props.onSelect} />
            {props.children}
        </div>
    ),
);
