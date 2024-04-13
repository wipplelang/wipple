import type { RuntimeComponent } from "..";
import { forwardRef, useEffect, useImperativeHandle, useRef, useState } from "react";
import turtleImage from "./turtle.png";
import { MaterialSymbol } from "react-material-symbols";
import { Tooltip } from "../../components";
import { format } from "date-fns";
import { PaletteItem } from "../../models";
import { flushSync } from "react-dom";
import { Resizable } from "react-resizable";

// @ts-ignore
import RealTurtle from "real-turtle";

export interface Settings {
    canvasWidth: number;
    canvasHeight: number;
}

const defaultSettings: Settings = {
    canvasWidth: 200,
    canvasHeight: 200,
};

const initializeTurtle = async (canvas: HTMLCanvasElement) => {
    const turtle = new RealTurtle(canvas, {
        async: true,
        image: turtleImage,
        state: {
            size: 30,
        },
    });

    await turtle.setLineWidth(2);

    const [ratio] = getPixelRatio(canvas);
    await turtle.setPosition(canvas.width / 2 / ratio, canvas.height / 2 / ratio);

    return turtle;
};

export const Turtle: RuntimeComponent<Settings> = forwardRef((props, ref) => {
    const containerRef = useRef<HTMLDivElement>(null);
    const canvasRef = useRef<HTMLCanvasElement>(null);

    useEffect(() => {
        const settings = props.settings ?? defaultSettings;

        containerRef.current!.style.width = `${settings.canvasWidth}px`;
        containerRef.current!.style.height = `${settings.canvasHeight}px`;
        canvasRef.current!.width = settings.canvasWidth;
        canvasRef.current!.height = settings.canvasHeight;

        rescaleCanvas(canvasRef.current!);
    }, []);

    const reset = async () => {
        const ctx = canvasRef.current!.getContext("2d")!;
        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, canvasRef.current!.width, canvasRef.current!.height);
        turtleRef.current = await initializeTurtle(canvasRef.current!);
    };

    const [resizable, setResizable] = useState(false);
    const [showResizedPrompt, setShowResizedPrompt] = useState(false);

    const [containerWidth, setContainerWidth] = useState(
        (props.settings ?? defaultSettings).canvasWidth,
    );

    const [containerHeight, setContainerHeight] = useState(
        (props.settings ?? defaultSettings).canvasHeight,
    );

    const [resizingImage, setResizingImage] = useState<{
        src: string;
        width: number;
        height: number;
    }>();

    const beginResize = ({ width, height }: { width: number; height: number }) => {
        const src = canvasRef.current!.toDataURL();
        setResizingImage({ src, width, height });
    };

    const onResize = ({ width, height }: { width: number; height: number }) => {
        setContainerWidth(width);
        setContainerHeight(height);
    };

    const endResize = async ({ width, height }: { width: number; height: number }) => {
        props.onChangeSettings({
            canvasWidth: width,
            canvasHeight: height,
        });

        canvasRef.current!.width = width;
        canvasRef.current!.height = height;
        rescaleCanvas(canvasRef.current!);

        await reset();

        setResizingImage(undefined);
    };

    const turtleRef = useRef<RealTurtle>();

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            flushSync(() => {
                setResizable(false);
                setShowResizedPrompt(false);
            });

            await endResize({
                width: containerRef.current!.clientWidth,
                height: containerRef.current!.clientHeight,
            });
        },
        onMessage: async (message, value) => {
            const turtle = turtleRef.current!;

            switch (message) {
                case "forward": {
                    await turtle.forward(value);
                    break;
                }
                case "backward": {
                    await turtle.back(value);
                    break;
                }
                case "arc": {
                    const [radius, angle] = value;
                    await turtle.arc(radius, angle);
                    break;
                }
                case "left": {
                    await turtle.left(value);
                    break;
                }
                case "right": {
                    await turtle.right(value);
                    break;
                }
                case "color": {
                    await turtle.setStrokeStyle(value);
                    break;
                }
                case "begin-path": {
                    await turtle.beginPath();
                    break;
                }
                case "end-path": {
                    await turtle.closePath();
                    await turtle.setFillStyle(value);
                    await turtle.fill();
                    break;
                }
                case "speed": {
                    await turtle.setSpeed(value);
                    break;
                }
                default: {
                    throw new Error(`unsupported message: ${message}`);
                }
            }
        },
        cleanup: async () => {
            turtleRef.current = undefined;
            setResizable(true);
        },
    }));

    const savePhoto = () => {
        if (!canvasRef.current) {
            return;
        }

        const image = canvasRef.current.toDataURL("image/png");
        const a = document.createElement("a");
        a.href = image;
        a.download = `turtle-${format(new Date(), "yyyyMMddHHmmss")}.png`;
        a.click();
    };

    return (
        <Resizable
            width={containerWidth}
            height={containerHeight}
            minConstraints={[200, 200]}
            maxConstraints={[600, 600]}
            onResizeStart={resizable ? (_event, data) => beginResize(data.size) : undefined}
            onResize={resizable ? (_event, data) => onResize(data.size) : undefined}
            onResizeStop={resizable ? (_event, data) => endResize(data.size) : undefined}
        >
            <div
                ref={containerRef}
                className={`relative rounded-md overflow-hidden border-2 border-gray-100 dark:border-gray-800`}
                style={{ width: containerWidth, height: containerHeight }}
            >
                <canvas ref={canvasRef} className="w-full h-full" />

                {resizingImage ? (
                    <>
                        <div className="absolute inset-0 bg-white" />

                        <img
                            src={resizingImage.src}
                            width={resizingImage.width}
                            height={resizingImage.height}
                            className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 object-cover"
                            style={{ width: resizingImage.width, height: resizingImage.height }}
                        />
                    </>
                ) : turtleRef.current?.[1] == null ? (
                    <div className="absolute top-0 right-0 transition-opacity">
                        <div className="flex flex-row items-center gap-2 p-2">
                            <Tooltip description="Save Photo">
                                <button
                                    className="flex items-center justify-center aspect-square p-1 bg-white hover:bg-gray-100 transition-colors border-2 border-gray-100 rounded-lg"
                                    onClick={savePhoto}
                                >
                                    <MaterialSymbol icon="photo_camera" size={18} color="black" />
                                </button>
                            </Tooltip>
                        </div>
                    </div>
                ) : null}
            </div>
        </Resizable>
    );
});

function getPixelRatio(ctx: any): [number, number] {
    let devicePixelRatio = window.devicePixelRatio || 1;

    let backingStoreRatio =
        ctx.webkitBackingStorePixelRatio ||
        ctx.mozBackingStorePixelRatio ||
        ctx.msBackingStorePixelRatio ||
        ctx.oBackingStorePixelRatio ||
        ctx.backingStorePixelRatio ||
        1;

    return [devicePixelRatio / backingStoreRatio, backingStoreRatio];
}

// https://www.keanw.com/2017/02/scaling-html-canvases-for-hidpi-screens.html
function rescaleCanvas(canvas: any) {
    // finally query the various pixel ratios

    let ctx = canvas.getContext("2d");
    let [ratio, backingStoreRatio] = getPixelRatio(ctx);

    // upscale the canvas if the two ratios don't match
    if (devicePixelRatio !== backingStoreRatio) {
        let oldWidth = canvas.width;
        let oldHeight = canvas.height;

        canvas.width = oldWidth * ratio;
        canvas.height = oldHeight * ratio;

        canvas.style.width = oldWidth + "px";
        canvas.style.height = oldHeight + "px";

        // now scale the context to counter
        // the fact that we've manually scaled
        // our canvas element

        ctx.scale(ratio, ratio);
    }
}

export { turtleImage };

export const paletteItems: PaletteItem[] = [
    {
        title: "forward",
        code: `forward ([Dropdown (10 , 20 , 30 , 40 , 50 , 100 , 200 , 500) 50] pixels)`,
    },
    {
        title: "backward",
        code: `backward ([Dropdown (10 , 20 , 30 , 40 , 50 , 100 , 200 , 500) 50] pixels)`,
    },
    {
        title: "left",
        code: `left ([Dropdown (10 , 20 , 30 , 45 , 60 , 90 , 180 , 270 , 360) 90] degrees)`,
    },
    {
        title: "right",
        code: `right ([Dropdown (10 , 20 , 30 , 45 , 60 , 90 , 180 , 270 , 360) 90] degrees)`,
    },
    {
        title: "color",
        code: `color [Color "#3b82f6"]`,
    },
    {
        title: "speed",
        code: `speed [Dropdown (very-slow , slow , normal , fast , very-fast) normal]`,
    },
    {
        title: "repeat",
        code: `repeat ([Dropdown (1 , 2 , 3 , 4 , 5 , 10 , 20 , 50 , 100) 1] times) {\n  _\n}`,
    },
];
