import type { RuntimeComponent } from "..";
import { forwardRef, useCallback, useEffect, useImperativeHandle, useRef, useState } from "react";
import turtleImage from "./assets/turtle.png";
import { MaterialSymbol } from "react-material-symbols";
import { defaultAnimationDuration, Tooltip } from "../../components";
import { format } from "date-fns";
import { PaletteCategory } from "../../models";
import { animalImageUrl } from "../../pages/edit/assets/animal";

// @ts-ignore
import RealTurtle from "real-turtle";
import { useStore } from "../../store";
import { Box } from "../../components/box";

const canvasSize = 500;
const canvasPixelRatio = 2.5;

const initializeTurtle = async (canvas: HTMLCanvasElement) => {
    const turtle = new RealTurtle(canvas, {
        async: true,
        image: turtleImage,
        state: {
            size: 30,
        },
    });

    await turtle.setLineWidth(2);

    await turtle.setPosition(canvasSize / 2, canvasSize / 2);

    return turtle;
};

export const Turtle: RuntimeComponent = forwardRef((props, ref) => {
    const [store, _setStore] = useStore();

    const [printingImage, setPrintingImage] = useState<string>();

    const isPrintingRef = useRef(store.isPrinting ?? false);
    useEffect(() => {
        if (store.isPrinting) {
            isPrintingRef.current = true;

            const image = canvasRef.current!.toDataURL("image/png");
            setPrintingImage(image);
        } else {
            setTimeout(() => {
                isPrintingRef.current = false;
                setPrintingImage(undefined);
            }, defaultAnimationDuration);
        }
    }, [store.isPrinting]);

    const containerRef = useRef<HTMLDivElement>(null);
    const canvasRef = useRef<HTMLCanvasElement>(null);

    const reset = useCallback(async () => {
        const canvas = canvasRef.current!;
        canvas.style.opacity = "0";
        rescaleCanvas(canvas);
        turtleRef.current = await initializeTurtle(canvas);
        canvas.style.opacity = "1";
    }, []);

    const turtleRef = useRef<RealTurtle>();

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            await reset();
        },
        onMessage: async (message, value) => {
            const turtle = turtleRef.current;
            if (!turtle) return;

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
                case "animal": {
                    await turtle.setImage(animalImageUrl(value));
                    break;
                }
                default: {
                    throw new Error(`unsupported message: ${message}`);
                }
            }
        },
        cleanup: async () => {
            turtleRef.current = undefined;
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

    const printingSize = "5in";

    return (
        <div
            ref={containerRef}
            className="aspect-square bg-white"
            style={{
                width: printingImage ? printingSize : undefined,
                height: printingImage ? printingSize : undefined,
            }}
        >
            <Box showBorderWhenPrinting>
                {printingImage ? (
                    <img src={printingImage} className="absolute inset-0 object-cover" />
                ) : (
                    <>
                        <canvas
                            ref={canvasRef}
                            width={canvasSize}
                            height={canvasSize}
                            className="absolute inset-0 size-full"
                        />

                        {turtleRef.current?.[1] == null && !store.isPrinting ? (
                            <div className="absolute top-0 right-0 transition-opacity">
                                <div className="flex flex-row items-center gap-2 p-2">
                                    <Tooltip description="Save Photo">
                                        <button
                                            className="flex items-center justify-center aspect-square p-1 bg-white hover:bg-gray-100 transition-colors border border-gray-100 rounded-lg"
                                            onClick={savePhoto}
                                        >
                                            <MaterialSymbol
                                                icon="photo_camera"
                                                size={18}
                                                color="black"
                                            />
                                        </button>
                                    </Tooltip>
                                </div>
                            </div>
                        ) : null}
                    </>
                )}
            </Box>
        </div>
    );
});

// Adapted from https://www.keanw.com/2017/02/scaling-html-canvases-for-hidpi-screens.html
const rescaleCanvas = (canvas: HTMLCanvasElement) => {
    const ctx = canvas.getContext("2d")!;
    canvas.width = canvasSize * canvasPixelRatio;
    canvas.height = canvasSize * canvasPixelRatio;
    ctx.scale(canvasPixelRatio, canvasPixelRatio);
};

export { turtleImage };

export const paletteCategories: PaletteCategory[] = [
    {
        title: "Movement",
        items: [
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
        ],
    },
    {
        title: "Appearance",
        items: [
            {
                title: "color",
                code: `color [Color "#3b82f6"]`,
            },
            {
                title: "speed",
                code: `speed [Slider 0.5 0 1]`,
            },
            {
                title: "animal",
                code: `animal [Animal "frog"]`,
            },
        ],
    },
    {
        title: "Control",
        items: [
            {
                title: "repeat",
                code: `repeat ([Dropdown (1 , 2 , 3 , 4 , 5 , 10 , 20 , 50 , 100) 2] times) {\n  _\n}`,
                replace: true,
            },
        ],
    },
];
