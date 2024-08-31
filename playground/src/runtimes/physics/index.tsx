import matter from "matter-js";
import { RuntimeComponent } from "..";
import { forwardRef, useCallback, useEffect, useImperativeHandle, useRef, useState } from "react";
import { PaletteItem } from "../../models";
import atomImage from "./assets/atom.png";
import { demonstrations } from "./demonstrations";
import { ContextMenuButton } from "../../components";
import { Mutex } from "async-mutex";
import { ObjectAsset } from "../../edit/assets/object";

export const worldWidth = 8;
export const worldHeight = 6;
const pixelRatio = 50;
const outOfBoundsThreshold = 1;

const defaultDelta = 1000 / 60;
let delta = defaultDelta; // default; will be updated to device's FPS in initializePhysics

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
    // Calculate device FPS
    const frameTimes: number[] = [];
    for (let i = 0; i < 10; i++) {
        const start = performance.now();
        await new Promise(requestAnimationFrame);
        frameTimes.push(performance.now() - start);
    }

    const averageFrameTime = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
    delta = defaultDelta ** 2 / averageFrameTime;

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
                force: (t: number) => Promise<matter.Vector>;
                measurements?: {
                    t: number;
                    m: number;
                    x: number;
                    y: number;
                    vx?: number;
                    vy?: number;
                    fx?: number;
                    fy?: number;
                };
                trail: matter.Vector[];
                hasUpdated: { current: boolean };
            }
        >
    >({});
    const observersRef = useRef<[number, () => void][]>([]);
    const mutexRef = useRef<Mutex>();

    const measure = (t: number, body: (typeof bodiesRef.current)[string]) => {
        const m = body.matterBody.mass;

        const x = body.matterBody.position.x - worldWidth / 2;
        const y = worldHeight / 2 - body.matterBody.position.y;

        if (!body.measurements) {
            return { t, m, x, y };
        }

        const dt = (t - body.measurements.t) * ms;

        const vx = ((x - body.measurements.x) / dt) * ms;
        const vy = ((y - body.measurements.y) / dt) * ms;

        const fx = body.measurements?.vx && ((vx - body.measurements.vx) / dt) * ms * m;
        const fy = body.measurements?.vy && ((vy - body.measurements.vy) / dt) * ms * m;

        return { t, m, x, y, vx, vy, fx, fy };
    };

    const reset = async () => {
        for (const [canvas, fillStyle] of [
            [backgroundRef.current, "white"],
            [canvasRef.current, "transparent"],
        ] as const) {
            if (!canvas) continue;

            resizeCanvas(canvas);
            const ctx = canvas.getContext("2d")!;
            ctx.fillStyle = fillStyle;
            ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
        }

        if (canvasRef.current) {
            engineRef.current = await initializePhysics(canvasRef.current);
        }

        observersRef.current = [];
        mutexRef.current = new Mutex();
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
                    force: async () => ({ x: 0, y: 0 }),
                    prevMeasurements: {
                        t: 0,
                        m: matterBody.mass,
                        x: matterBody.position.x - worldWidth / 2,
                        y: worldHeight / 2 - matterBody.position.y,
                        vx: 0,
                        vy: 0,
                        fx: 0,
                        fy: 0,
                    },
                    measurements: undefined,
                    trail: [],
                    hasUpdated: { current: false },
                },
            ]),
        );

        matter.Composite.add(engine.world, Object.values(demonstration.bodies));
    }, [demonstrationName]);

    useEffect(() => {
        if (!demonstrationName) return;

        (async () => {
            await reset();
            setDemonstration();
        })();
    }, [demonstrationName]);

    const updateTrail = useCallback(() => {
        const ctx = backgroundRef.current!.getContext("2d")!;
        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);

        const dotSize = 3;
        const dotInterval = 4;
        for (const { matterBody, trail } of Object.values(bodiesRef.current)) {
            if (trail.length <= 1) continue;

            ctx.globalAlpha = 0.125;
            ctx.strokeStyle = matterBody.render.fillStyle!;
            ctx.lineWidth = dotSize;
            ctx.beginPath();
            ctx.moveTo(trail[0].x * pixelRatio, trail[0].y * pixelRatio);
            for (const { x, y } of trail.slice(1)) {
                ctx.lineTo(x * pixelRatio, y * pixelRatio);
            }
            ctx.stroke();
            ctx.globalAlpha = 1;

            ctx.fillStyle = matterBody.render.fillStyle!;

            trail.forEach(({ x, y }, i) => {
                if (i % dotInterval === 0) {
                    ctx.fillRect(
                        x * pixelRatio - dotSize / 2,
                        y * pixelRatio - dotSize / 2,
                        dotSize,
                        dotSize,
                    );
                }
            });
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
                    if (
                        Object.values(bodiesRef.current).every(
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
                        let t = engine.timing.timestamp / ms;

                        observersRef.current = observersRef.current.filter(([time, callback]) => {
                            if (t >= time) {
                                callback();
                                return false;
                            }

                            return true;
                        });

                        await new Promise(requestAnimationFrame);

                        for (const body of Object.values(bodiesRef.current)) {
                            const force = await body.force(t);

                            matter.Body.applyForce(
                                body.matterBody,
                                body.matterBody.position,
                                force,
                            );
                        }

                        matter.Engine.update(engine, delta, 1);

                        t = engine.timing.timestamp / ms;

                        for (const body of Object.values(bodiesRef.current)) {
                            if (
                                body.hasUpdated.current &&
                                (body.trail[body.trail.length - 1]?.x !==
                                    body.matterBody.position.x ||
                                    body.trail[body.trail.length - 1]?.y !==
                                        body.matterBody.position.y)
                            ) {
                                body.trail.push({ ...body.matterBody.position });
                            }

                            body.hasUpdated.current = true;

                            body.measurements = measure(t, body);
                        }

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
                        await mutexRef.current?.runExclusive(async () => {
                            const t = engine.timing.timestamp / ms;
                            const x = await fx([t]);
                            const y = await fy([t]);

                            matter.Body.setPosition(body.matterBody, {
                                x: isNaN(x) ? body.matterBody.position.x : x + worldWidth / 2,
                                y: isNaN(y) ? body.matterBody.position.y : worldHeight / 2 - y,
                            });
                        });
                    });

                    break;
                }
                case "force": {
                    const engine = engineRef.current;
                    if (!engine) return;

                    const bodies = bodiesRef.current;

                    const [bodyName, fx, fy] = value;

                    const body = bodies[bodyName];
                    if (!body) return;

                    body.force = async (t) => {
                        if (!mutexRef.current) {
                            return { x: 0, y: 0 };
                        }

                        return mutexRef.current!.runExclusive(async () => {
                            const x = await fx([t]);
                            const y = await fy([t]);

                            return {
                                x: isNaN(x) ? 0 : x / (ms * ms),
                                y: isNaN(y) ? 0 : -y / (ms * ms),
                            };
                        });
                    };

                    break;
                }
                case "observe": {
                    const observers = observersRef.current;

                    const [time, block] = value;

                    const callback = () =>
                        mutexRef.current?.runExclusive(async () => {
                            await block([]);
                        });

                    observers.push([time, callback]);

                    break;
                }
                case "time": {
                    const engine = engineRef.current;
                    if (!engine) return 0;

                    return engine.timing.timestamp / ms;
                }
                case "measure": {
                    const engine = engineRef.current;
                    if (!engine) return [0, 0, 0, 0, 0, 0, 0];

                    const bodies = bodiesRef.current;

                    const bodyName = value;

                    const body = bodies[bodyName];
                    if (!body?.measurements) return [body.matterBody.mass, 0, 0, 0, 0, 0, 0];

                    const { m, x, y, vx, vy, fx, fy } = body.measurements;

                    return [
                        round(m),
                        round(x),
                        round(y),
                        round(vx ?? 0),
                        round(vy ?? 0),
                        round(fx ?? 0),
                        round(fy ?? 0),
                    ];
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
            observersRef.current = [];
            mutexRef.current = undefined;
        },
    }));

    return (
        <div
            ref={containerRef}
            className={`relative rounded-md overflow-hidden border-2 border-gray-100 dark:border-gray-800`}
            style={{ width: worldWidth * pixelRatio, height: worldHeight * pixelRatio }}
        >
            {demonstrationName ? (
                <>
                    <canvas ref={backgroundRef} className="absolute inset-0 w-full h-full" />
                    <canvas ref={canvasRef} className="absolute inset-0 w-full h-full" />
                </>
            ) : (
                <div className="absolute inset-0 flex items-center justify-center w-full h-full">
                    <ContextMenuButton
                        items={Object.keys(demonstrations).map((demonstrationName) => ({
                            title: demonstrationName,
                            onClick: () => {
                                props.onChangeSettings({
                                    demonstration: demonstrationName,
                                });

                                setDemonstrationName(demonstrationName);
                            },
                        }))}
                    >
                        <div className="text-gray-500 dark:text-gray-400">
                            Choose a Demonstration
                        </div>
                    </ContextMenuButton>
                </div>
            )}
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

const round = (n: number) => parseFloat(n.toFixed(4));

export { atomImage };

export const paletteItems: PaletteItem[] = [
    {
        title: "position",
        code: `position [Object "Block 1"] x y`,
    },
    {
        title: "force",
        code: `force [Object "Block 1"] fx fy`,
    },
    {
        title: "observe",
        code: `observe (1 seconds) {\n\n}`,
    },
];
