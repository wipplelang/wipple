import matter from "matter-js";
import { RuntimeComponent } from "..";
import { forwardRef, useCallback, useEffect, useImperativeHandle, useRef, useState } from "react";
import { PaletteItem } from "../../models";
import { ColorAsset } from "../../pages/edit/assets/color";
import atomImage from "./assets/atom.png";
import { demonstrations } from "./demonstrations";

export const worldWidth = 8;
export const worldHeight = 6;
const pixelRatio = 50;

// https://github.com/liabru/matter-js/issues/666#issuecomment-615939507
const ms = 1000;

const delta = 10; // ms

// @ts-ignore (don't multiply force, velocity, etc. by anything)
matter.Common._baseDelta = 1;
// @ts-ignore
matter.Body._baseDelta = 1;

// @ts-ignore (https://github.com/liabru/matter-js/issues/256#issuecomment-907964224 and https://github.com/liabru/matter-js/issues/394#issuecomment-289913662)
matter.Resolver._restingThresh = 0.001;

export interface Settings {
    demonstration: string | null;
}

const defaultSettings: Settings = {
    demonstration: null,
};

const initializePhysics = async (canvas: HTMLCanvasElement) => {
    const engine = matter.Engine.create({
        gravity: { y: 0 }, // let the user define gravity!
    });

    const render = matter.Render.create({
        canvas,
        engine,
        options: {
            width: worldWidth,
            height: worldHeight,
            pixelRatio,
            background: "white",
            wireframes: false,
        },
    });

    canvas.width = worldWidth * pixelRatio;
    canvas.height = worldHeight * pixelRatio;
    canvas.style.width = `${worldWidth * pixelRatio}px`;
    canvas.style.height = `${worldHeight * pixelRatio}px`;

    matter.Render.run(render);

    return engine;
};

export const Physics: RuntimeComponent<Settings> = forwardRef((props, ref) => {
    const containerRef = useRef<HTMLDivElement>(null);
    const canvasRef = useRef<HTMLCanvasElement>(null);

    const engineRef = useRef<matter.Engine>();
    const bodiesRef = useRef<Record<string, matter.Body>>({});

    const reset = async () => {
        const ctx = canvasRef.current!.getContext("2d")!;
        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, canvasRef.current!.width, canvasRef.current!.height);
        engineRef.current = await initializePhysics(canvasRef.current!);
    };

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            await reset();
            setDemonstration();
        },
        onMessage: async (message, value) => {
            const engine = engineRef.current;
            if (!engine) return;

            const bodies = bodiesRef.current;

            switch (message) {
                case "x": {
                    console.log("value:", value);

                    const [bodyName, func] = value;

                    const body = bodies[bodyName];
                    if (!body) {
                        return;
                    }

                    console.log(bodyName, func);

                    matter.Events.on(engine, "beforeUpdate", async () => {
                        const t = engine.timing.timestamp / ms;
                        const x = await props.call(func, t);

                        console.warn({ x });

                        matter.Body.setPosition(body, {
                            x: isNaN(x) ? body.position.x : x + worldWidth / 2,
                            y: body.position.y,
                        });
                    });

                    break;
                }
                case "y": {
                    const [bodyName, func] = value;

                    const body = bodies[bodyName];
                    if (!body) {
                        return;
                    }

                    matter.Events.on(engine, "beforeUpdate", async () => {
                        const t = engine.timing.timestamp / ms;
                        const y = await props.call(func, t);

                        matter.Body.setPosition(body, {
                            x: body.position.x,
                            y: isNaN(y) ? body.position.y : worldWidth / 2 - y,
                        });
                    });

                    break;
                }
                case "apply-force": {
                    const [bodyIndex, x, y] = value;
                    const body = bodies[bodyIndex];

                    matter.Body.applyForce(body, body.position, {
                        x: x / (ms * ms),
                        y: -y / (ms * ms),
                    });

                    break;
                }
                case "set-gravity": {
                    const [x, y] = value;
                    engine.gravity = {
                        scale: 1,
                        x: x / (ms * ms),
                        y: -y / (ms * ms),
                    };

                    break;
                }
                default: {
                    throw new Error(`unsupported message: ${message}`);
                }
            }
        },
        cleanup: async () => {
            console.warn("cleaning up");

            if (engineRef.current) {
                matter.Render.stop(engineRef.current.render);
                matter.Engine.clear(engineRef.current);
            }

            engineRef.current = undefined;
        },
    }));

    const [demonstrationName, setDemonstrationName] = useState(
        (props.settings ?? defaultSettings).demonstration,
    );

    const setDemonstration = useCallback(() => {
        const engine = engineRef.current;
        if (!engine) return;

        matter.World.clear(engine.world, false);

        if (!demonstrationName) {
            console.error("No demonstration set");
            return;
        }

        const demonstration = demonstrations[demonstrationName]?.();
        if (!demonstration) {
            console.error("Invalid demonstration", demonstrationName);
            return;
        }

        console.log("Initializing demonstration", demonstrationName, demonstration);

        bodiesRef.current = demonstration.bodies;

        matter.Composite.add(engine.world, Object.values(demonstration.bodies));

        const update = () => {
            if (engineRef.current !== engine) return;

            matter.Engine.update(engine, delta);
            console.log(engine.world.bodies);
            requestAnimationFrame(update);
        };

        update();
    }, [demonstrationName]);

    useEffect(() => {
        props.onChangeSettings({
            ...(props.settings ?? defaultSettings),
            demonstration: demonstrationName,
        });

        setDemonstration();
    }, [demonstrationName]);

    useEffect(() => {
        if (!props.settings?.demonstration) {
            setDemonstrationName("Single Block");
        }
    }, []);

    return (
        <div
            ref={containerRef}
            className={`relative rounded-md overflow-hidden border-2 border-gray-100 dark:border-gray-800`}
            style={{ width: worldWidth * pixelRatio, height: worldHeight * pixelRatio }}
        >
            <canvas ref={canvasRef} className="w-full h-full" />
        </div>
    );
});

export { atomImage };

export const assetItems: PaletteItem[] = [
    {
        title: <ColorAsset color="#3b82f6" tooltip={false} />,
        code: `[Color "#3b82f6"]`,
    },
];

export const paletteItems: PaletteItem[] = [
    {
        title: "repeat",
        code: `repeat ([Dropdown (1 , 2 , 3 , 4 , 5 , 10 , 20 , 50 , 100) 1] times) {\n  _\n}`,
    },

    // TODO
];
