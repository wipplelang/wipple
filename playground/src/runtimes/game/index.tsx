import type { RuntimeComponent } from "..";
import { forwardRef, useImperativeHandle, useRef, useState } from "react";
import { PaletteItem } from "../../models";
import { Resizable } from "re-resizable";
import { ColorAsset } from "../../edit/assets/color";
import * as engine from "./engine";
import "./assets/style.css";
import { MaterialSymbol } from "react-material-symbols";
import { ResizeHandle, Tooltip } from "../../components";

export interface Settings {
    canvasWidth: number;
    canvasHeight: number;
}

const defaultSettings: Settings = {
    canvasWidth: 600,
    canvasHeight: 450,
};

export const Game: RuntimeComponent<Settings> = forwardRef((props, ref) => {
    const [onInitialized, setOnInitialized] = useState<() => void>();

    const [containerWidth, setContainerWidth] = useState(
        (props.settings ?? defaultSettings).canvasWidth,
    );

    const [containerHeight, setContainerHeight] = useState(
        (props.settings ?? defaultSettings).canvasHeight,
    );

    const containerRef = useRef<HTMLDivElement>(null);

    const canvas = useRef<HTMLCanvasElement | null>(null);
    const ctx = useRef<engine.GameContext | null>(null as any);
    const commit = useRef<((flags: { shouldContinue: boolean }) => Promise<void>) | null>(
        null as any,
    );

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            for (const child of containerRef.current!.children) {
                containerRef.current!.removeChild(child);
            }

            await new Promise<void>(async (resolve) => {
                setOnInitialized(() => resolve);
            });

            setOnInitialized(undefined);

            const scene: engine.Scene = (newCtx, newCommit) => {
                ctx.current = newCtx;
                commit.current = newCommit;
            };

            const input = engine.inputs.combined(engine.inputs.gamepad(), engine.inputs.keyboard());

            canvas.current = await engine.backends.ptc.run(scene, input, containerRef.current!);
        },
        onMessage: async (message, value) => {
            if (!ctx.current || !commit.current) return;

            switch (message) {
                case "locate": {
                    const [x, y] = value;
                    ctx.current.locate(x, y);
                    return null;
                }
                case "fg": {
                    ctx.current.fg(value);
                    return null;
                }
                case "bg": {
                    ctx.current.bg(value);
                    return null;
                }
                case "render-glyph": {
                    ctx.current.printGlyph(value);
                    return null;
                }
                case "render-line": {
                    ctx.current.print(value);
                    return null;
                }
                case "render": {
                    ctx.current.print(value, false);
                    return null;
                }
                case "render-line-fill": {
                    const [string, replacement] = value;
                    ctx.current.printf(string, replacement);
                    return null;
                }
                case "render-fill": {
                    const [string, replacement] = value;
                    ctx.current.printf(string, replacement, false);
                    return null;
                }
                case "play": {
                    const [song, loop] = value;
                    ctx.current.play(new engine.Music(song, loop === 1));
                    return null;
                }
                case "pause": {
                    ctx.current.pause();
                    return null;
                }
                case "button": {
                    const button = await ctx.current.button();
                    return button;
                }
                case "commit":
                    await commit.current({ shouldContinue: true });
                    return null;
                default:
                    throw new Error("unknown message");
            }
        },
        cleanup: async () => {
            await commit.current?.({ shouldContinue: false });
            ctx.current = null;
            commit.current = null;
        },
    }));

    return (
        <Resizable
            size={{ width: containerWidth, height: containerHeight }}
            minWidth={200}
            minHeight={150}
            maxWidth={700}
            maxHeight={525}
            lockAspectRatio
            onResize={(_event, _direction, element) => {
                setContainerWidth(element.clientWidth);
                setContainerHeight(element.clientHeight);
            }}
            onResizeStop={(_event, _direction, element) => {
                if (
                    element.clientWidth !== props.settings?.canvasWidth ||
                    element.clientHeight !== props.settings?.canvasHeight
                ) {
                    props.onChangeSettings({
                        canvasWidth: element.clientWidth,
                        canvasHeight: element.clientHeight,
                    });
                }
            }}
            handleComponent={{ bottomRight: <ResizeHandle /> }}
        >
            <div
                className={`group relative rounded-md overflow-hidden border-2 border-gray-100 dark:border-gray-800`}
                style={{ width: containerWidth, height: containerHeight, aspectRatio: 4 / 3 }}
            >
                <div ref={containerRef} id="game" className="absolute inset-0" />

                {onInitialized ? (
                    <div className="absolute inset-0 flex flex-col items-center justify-center bg-black">
                        <button
                            className="px-4 py-2 text-black bg-white hover:opacity-95 rounded-md transition-opacity"
                            onClick={onInitialized}
                        >
                            Start
                        </button>
                    </div>
                ) : (
                    <div className="absolute top-0 right-0 opacity-0 group-hover:opacity-100 transition-opacity">
                        <div className="flex flex-row items-center gap-2 p-2">
                            <Tooltip description="Fullscreen">
                                <button
                                    className="flex items-center justify-center aspect-square p-1 bg-black hover:bg-gray-900 transition-colors border-2 border-gray-800 rounded-lg"
                                    onClick={() => {
                                        containerRef.current!.requestFullscreen({
                                            navigationUI: "hide",
                                        });
                                    }}
                                >
                                    <MaterialSymbol icon="fullscreen" size={18} color="white" />
                                </button>
                            </Tooltip>
                        </div>
                    </div>
                )}
            </div>
        </Resizable>
    );
});

export const paletteItems: PaletteItem[] = [
    {
        title: "locate",
        code: "locate (x ; y)",
    },
    {
        title: "fg",
        code: 'fg [Color "#3b82f6"]',
    },
    {
        title: "bg",
        code: 'bg [Color "#3b82f6"]',
    },
    {
        title: "render-line",
        code: 'render-line "Wipple"',
    },
    {
        title: "render-line-fill",
        code: 'render-line-fill "..."',
    },
    {
        title: "button",
        code: "button",
    },
    {
        title: "frame",
        code: "frame {\n  _\n}",
        replace: true,
    },
];
