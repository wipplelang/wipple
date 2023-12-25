import matter from "matter-js";

const worldWidth = 8;
const worldHeight = 6;
const pixelRatio = 100;

// https://kdesign.co/blog/pastel-color-palette-examples/
const colors = ["#F7D9C4", "#C9E4DE", "#C6DEF1", "#F2C6DE", "#DBCDFO", "#FAEDCB"];

// https://github.com/liabru/matter-js/issues/666#issuecomment-615939507
const ms = 1000;

const delta = 10; // ms

// @ts-ignore (don't multiply force, velocity, etc. by anything)
matter.Common._baseDelta = 1;
// @ts-ignore
matter.Body._baseDelta = 1;

// @ts-ignore (https://github.com/liabru/matter-js/issues/256#issuecomment-907964224 and https://github.com/liabru/matter-js/issues/394#issuecomment-289913662)
matter.Resolver._restingThresh = 0.001;

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = async (id: string, container: HTMLElement) => {
    const canvas = document.createElement("canvas");
    container.appendChild(canvas);

    const engine = matter.Engine.create({
        gravity: { y: 0 }, // let the user define gravity!
    });

    const render = matter.Render.create({
        canvas,
        engine,
        options: {
            width: worldWidth,
            height: worldHeight,
            pixelRatio: 100,
            background: "white",
            wireframes: false,
        },
    });

    canvas.width = worldWidth * pixelRatio;
    canvas.height = worldHeight * pixelRatio;
    canvas.style.width = `${worldWidth * pixelRatio}px`;
    canvas.style.height = `${worldHeight * pixelRatio}px`;

    matter.Render.run(render);

    const bodies: matter.Body[] = [];

    onMessage[id] = async (message, value) => {
        try {
            switch (message) {
                case "clock":
                    return engine.timing.timestamp / ms;
                case "within-bounds": {
                    const threshold = value;
                    const bodies = engine.world.bodies.filter((body) => !body.isStatic);

                    return (
                        bodies.length === 0 ||
                        bodies.some(
                            (body) =>
                                body.position.x >= -threshold &&
                                body.position.x <= worldWidth + threshold &&
                                body.position.y >= -threshold &&
                                body.position.y <= worldHeight + threshold
                        )
                    );
                }
                case "create-object": {
                    let [
                        shape,
                        color,
                        width,
                        height,
                        centerX,
                        centerY,
                        mass,
                        restitution,
                        rotates,
                        solid,
                        x,
                        y,
                    ] = value;

                    const options: matter.IChamferableBodyDefinition = {
                        render: { fillStyle: color || colors[bodies.length % colors.length] },

                        // @ts-ignore (don't multiply force, velocity, etc. by anything)
                        deltaTime: 1,

                        isStatic: isNaN(mass),
                        restitution,
                        inertia: rotates ? undefined : Infinity,
                        isSensor: !solid,

                        // TODO: Let user specify these
                        friction: 0,
                        frictionAir: 0,
                        frictionStatic: 0,
                    };

                    x = isNaN(x) ? worldWidth / 2 : x + worldWidth / 2;
                    y = isNaN(y) ? worldHeight / 2 : worldHeight / 2 - y;

                    let body: matter.Body;
                    switch (shape) {
                        case "box":
                            body = matter.Bodies.rectangle(x, y, width, height, options);
                            break;
                        // circles are a bit broken; they don't respect 'setCentre'
                        default:
                            throw new Error("invalid shape");
                    }

                    matter.Body.setCentre(
                        body,
                        { x: centerX * (width / 2), y: -centerY * (height / 2) },
                        true
                    );

                    if (!isNaN(mass)) {
                        matter.Body.setMass(body, mass);
                    }

                    const bodyIndex = bodies.length;
                    bodies.push(body);

                    matter.Composite.add(engine.world, [body]);

                    return bodyIndex;
                }
                case "set-position": {
                    const [bodyIndex, x, y] = value;
                    const body = bodies[bodyIndex];

                    matter.Body.setPosition(body, {
                        x: isNaN(x) ? body.position.x : x + worldWidth / 2,
                        y: isNaN(y) ? body.position.y : worldHeight / 2 - y,
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
                case "position-x": {
                    const bodyIndex = value;
                    const body = bodies[bodyIndex];
                    return body.position.x - worldWidth / 2;
                }
                case "position-y": {
                    const bodyIndex = value;
                    const body = bodies[bodyIndex];
                    return body.position.y + worldHeight / 2;
                }
                case "velocity-x": {
                    const bodyIndex = value;
                    const body = bodies[bodyIndex];
                    return body.velocity.x * ms;
                }
                case "velocity-y": {
                    const bodyIndex = value;
                    const body = bodies[bodyIndex];
                    return body.velocity.y * ms;
                }
                case "speed": {
                    const bodyIndex = value;
                    const body = bodies[bodyIndex];
                    return body.speed * ms;
                }
                case "force-x": {
                    const bodyIndex = value;
                    const body = bodies[bodyIndex];
                    return body.force.x * (ms * ms);
                }
                case "force-y": {
                    const bodyIndex = value;
                    const body = bodies[bodyIndex];
                    return body.force.y * (ms * ms);
                }
                case "tick": {
                    const updatePromise = new Promise<void>((resolve) => {
                        const handler = () => {
                            resolve();
                            matter.Events.off(engine, "afterUpdate", handler);
                        };

                        matter.Events.on(engine, "afterUpdate", handler);
                    });

                    requestAnimationFrame(() => {
                        matter.Engine.update(engine, delta);
                        console.log(engine.world.bodies);
                    });

                    await updatePromise;

                    break;
                }
                default: {
                    throw new Error("unknown message");
                }
            }
        } catch (error) {
            console.error("[physics] error:", error);
        }
    };
};

export const cleanup = async (id: string) => {
    delete onMessage[id];
};

// https://www.keanw.com/2017/02/scaling-html-canvases-for-hidpi-screens.html
function rescaleCanvas(canvas: any) {
    // finally query the various pixel ratios

    let ctx = canvas.getContext("2d");

    let devicePixelRatio = window.devicePixelRatio || 1;

    let backingStoreRatio =
        ctx.webkitBackingStorePixelRatio ||
        ctx.mozBackingStorePixelRatio ||
        ctx.msBackingStorePixelRatio ||
        ctx.oBackingStorePixelRatio ||
        ctx.backingStorePixelRatio ||
        1;

    let ratio = devicePixelRatio / backingStoreRatio;

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
