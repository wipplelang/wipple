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
const outOfBoundsThreshold = 1;

// https://github.com/liabru/matter-js/issues/666#issuecomment-615939507
const ms = 1000;

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

const resizeCanvas = (canvas: HTMLCanvasElement) => {
    canvas.width = worldWidth * pixelRatio;
    canvas.height = worldHeight * pixelRatio;
    canvas.style.width = `${worldWidth * pixelRatio}px`;
    canvas.style.height = `${worldHeight * pixelRatio}px`;

    rescaleCanvas(canvas);
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
            pixelRatio: pixelRatio * getPixelRatio(canvas.getContext("2d"))[0],
            background: "transparent",
            wireframes: false,
        },
    });

    engine.render = render;

    resizeCanvas(canvas);

    matter.Render.run(render);

    return engine;
};

export const Physics: RuntimeComponent<Settings> = forwardRef((props, ref) => {
    const containerRef = useRef<HTMLDivElement>(null);
    const backgroundRef = useRef<HTMLCanvasElement>(null);
    const canvasRef = useRef<HTMLCanvasElement>(null);

    const lastUpdateTimeRef = useRef<number | undefined>();
    const engineRef = useRef<matter.Engine>();
    const bodiesRef = useRef<
        Record<
            string,
            {
                matterBody: matter.Body;
                trail: matter.Vector[];
            }
        >
    >({});

    const reset = async () => {
        for (const canvas of [backgroundRef.current, canvasRef.current]) {
            resizeCanvas(canvas!);
            const ctx = canvas!.getContext("2d")!;
            ctx.fillStyle = "transparent";
            ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
        }

        engineRef.current = await initializePhysics(canvasRef.current!);
    };

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

        bodiesRef.current = Object.fromEntries(
            Object.entries(demonstration.bodies).map(([name, matterBody]) => [
                name,
                {
                    matterBody,
                    trail: [],
                },
            ]),
        );

        matter.Composite.add(engine.world, Object.values(demonstration.bodies));
    }, [demonstrationName]);

    useEffect(() => {
        setDemonstration();
    }, [demonstrationName]);

    useEffect(() => {
        if (!props.settings?.demonstration) {
            setDemonstrationName("Single Block");
        }
    }, []);

    const updateTrail = useCallback(() => {
        const ctx = backgroundRef.current!.getContext("2d")!;
        ctx.fillStyle = "transparent";
        ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);

        for (const { matterBody, trail } of Object.values(bodiesRef.current)) {
            if (trail.length <= 1) continue;

            ctx.strokeStyle = matterBody.render.fillStyle!;
            ctx.lineWidth = 2;

            ctx.beginPath();
            ctx.moveTo(trail[0].x * pixelRatio, trail[0].y * pixelRatio);
            for (const { x, y } of trail.slice(1)) {
                ctx.lineTo(x * pixelRatio, y * pixelRatio);
            }
            ctx.stroke();
        }
    }, []);

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            await reset();
            setDemonstration();
        },
        onMessage: async (message, value) => {
            switch (message) {
                case "tick": {
                    const now = performance.now();
                    const delta = lastUpdateTimeRef.current ? now - lastUpdateTimeRef.current : 0;
                    lastUpdateTimeRef.current = now;

                    if (
                        Object.values(bodiesRef.current).some(
                            ({ matterBody: body }) =>
                                body.position.x < -outOfBoundsThreshold ||
                                body.position.x > worldWidth + outOfBoundsThreshold ||
                                body.position.y < -outOfBoundsThreshold ||
                                body.position.y > worldHeight + outOfBoundsThreshold,
                        )
                    ) {
                        return false;
                    }

                    const engine = engineRef.current;

                    if (engine) {
                        await new Promise(requestAnimationFrame);
                        matter.Engine.update(engine, delta);
                        updateTrail();
                    }

                    return true;
                }
                case "position": {
                    const engine = engineRef.current;
                    if (!engine) return;

                    const bodies = bodiesRef.current;

                    const [bodyName, fx, fy] = value;

                    const body = bodies[bodyName];
                    if (!body) return;

                    matter.Events.on(engine, "beforeUpdate", async () => {
                        const t = engine.timing.timestamp / ms;
                        const x = await props.call(fx, t);
                        const y = await props.call(fy, t);

                        const position = {
                            x: isNaN(x) ? body.matterBody.position.x : x + worldWidth / 2,
                            y: isNaN(y) ? body.matterBody.position.y : worldHeight / 2 - y,
                        };

                        matter.Body.setPosition(body.matterBody, position);

                        body.trail.push(position);
                        if (body.trail.length > 100) {
                            body.trail.shift();
                        }
                    });

                    break;
                }
                case "apply-force": {
                    const engine = engineRef.current;
                    if (!engine) return;

                    const bodies = bodiesRef.current;

                    const [bodyName, x, y] = value;

                    const body = bodies[bodyName];
                    if (!body) return;

                    matter.Body.applyForce(body.matterBody, body.matterBody.position, {
                        x: x / (ms * ms),
                        y: -y / (ms * ms),
                    });

                    break;
                }
                case "set-gravity": {
                    const engine = engineRef.current;
                    if (!engine) return;

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
            if (engineRef.current?.enabled) {
                matter.Render.stop(engineRef.current.render);
            }

            engineRef.current = undefined;
            lastUpdateTimeRef.current = undefined;
        },
    }));

    return (
        <div
            ref={containerRef}
            className={`relative rounded-md overflow-hidden border-2 border-gray-100 dark:border-gray-800`}
            style={{ width: worldWidth * pixelRatio, height: worldHeight * pixelRatio }}
        >
            <canvas ref={backgroundRef} className="absolute inset-0 w-full h-full" />
            <canvas ref={canvasRef} className="absolute inset-0 w-full h-full" />
        </div>
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