import ReactDOM from "react-dom/client";
import { Compartment, Extension } from "@codemirror/state";
import { GutterMarker, gutter } from "@codemirror/view";
import { useHover } from "usehooks-ts";
import { useRef } from "react";
import { animated, useSpring } from "@react-spring/web";
import { MaterialSymbol } from "react-material-symbols";

export const gutterMenu = new Compartment();

export const gutterMenuFromConfig = (onClickLine: (line: number) => void): Extension => [
    gutter({
        lineMarker: (view, line) => {
            if (
                // view.hasFocus &&
                view.state.selection.main.empty &&
                view.state.selection.main.head >= line.from &&
                view.state.selection.main.head <= line.to
            ) {
                const lineNumber = view.state.doc.lineAt(line.from).number;
                return new MenuGutterMarker(lineNumber, line.height, () => onClickLine(lineNumber));
            } else {
                return null;
            }
        },
        lineMarkerChange: () => true,
    }),
];

class MenuGutterMarker extends GutterMarker {
    private root?: ReactDOM.Root;

    constructor(private lineNumber: number, private height: number, private onClick: () => void) {
        super();
    }

    eq(other: this) {
        return (
            this.lineNumber === other.lineNumber &&
            this.height === other.height &&
            this.onClick === other.onClick
        );
    }

    toDOM(): HTMLElement {
        const container = document.createElement("div");

        this.root = ReactDOM.createRoot(container);
        this.root.render(<MenuGutterMarkerComponent height={this.height} onClick={this.onClick} />);

        return container;
    }

    destroy() {
        requestAnimationFrame(() => {
            this.root?.unmount();
        });
    }
}

const MenuGutterMarkerComponent = (props: { height: number; onClick: () => void }) => {
    const ref = useRef<HTMLDivElement>(null);
    const isHovering = useHover(ref);

    const style = useSpring({
        width: isHovering ? "11pt" : "2pt",
        height: props.height,
        config: { tension: 600, friction: 60, bounce: 0 },
    });

    return (
        <div ref={ref} className="w-[6pt]">
            <animated.div style={style}>
                <button
                    // onClick={props.onClick}
                    onClick={() => alert("HI")}
                    className={`block relative w-full h-full bg-blue-500 rounded-r-md ${
                        isHovering ? "shadow-md shadow-blue-200 dark:shadow-blue-950" : ""
                    }`}
                >
                    <div
                        className={`flex items-center justify-center absolute top-0 bottom-0 right-0 transition-opacity ${
                            isHovering ? "opacity-100" : "opacity-0"
                        }`}
                    >
                        <MaterialSymbol icon="add" className="text-white" />
                    </div>
                </button>
            </animated.div>
        </div>
    );
};
