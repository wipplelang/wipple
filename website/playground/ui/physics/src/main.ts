import matter from "matter-js";

// @ts-ignore (https://github.com/liabru/matter-js/issues/256#issuecomment-907964224 and https://github.com/liabru/matter-js/issues/394#issuecomment-289913662)
matter.Resolver._restingThresh = 0.001;

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = async (id: string, container: HTMLElement) => {
    const element = document.createElement("div");
    container.appendChild(element);

    const worldWidth = 800;
    const worldHeight = 600;
    const massScale = 100000;

    const engine = matter.Engine.create({
        gravity: { y: 0 }, // let the user define gravity!
        positionIterations: 50,
        velocityIterations: 50,
    });

    const render = matter.Render.create({
        element,
        engine,
        options: {
            width: worldWidth,
            height: worldHeight,
            background: "white",
            wireframes: false,
            showPositions: true,
            showVelocity: true,
        },
    });

    matter.Render.run(render);

    const runner = matter.Runner.create();
    matter.Runner.run(runner, engine);

    const bodies: matter.Body[] = [];

    const start = new Date();

    onMessage[id] = async (message, value) => {
        try {
            switch (message) {
                case "clock":
                    return (new Date().valueOf() - start.valueOf()) / 1000;
                case "within-bounds":
                    return engine.world.bodies
                        .filter((body) => !body.isStatic)
                        .some(
                            (body) =>
                                body.position.x >= -value &&
                                body.position.x <= worldWidth + value &&
                                body.position.y >= -value &&
                                body.position.y <= worldHeight + value
                        );
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
                        x,
                        y,
                    ] = value;

                    const options: matter.IChamferableBodyDefinition = {
                        render: { fillStyle: color },

                        isStatic: isNaN(mass),
                        restitution,

                        // TODO: Let user specify these
                        inertia: rotates ? undefined : Infinity,
                        friction: 0,
                        frictionAir: 0,
                        frictionStatic: 0,
                    };

                    x = isNaN(x) ? worldWidth / 2 : x;
                    y = isNaN(y) ? worldHeight / 2 : worldHeight - y;

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
                        matter.Body.setMass(body, mass * massScale);
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
                        x: isNaN(x) ? body.position.x : x,
                        y: isNaN(y) ? body.position.y : worldHeight - y,
                    });

                    break;
                }
                case "apply-force": {
                    const [bodyIndex, x, y] = value;
                    const body = bodies[bodyIndex];

                    matter.Body.applyForce(body, body.position, { x: x, y: -y });

                    break;
                }
                case "set-gravity": {
                    const [x, y] = value;
                    engine.gravity = {
                        scale: 1,
                        x: x / massScale,
                        y: -y / massScale,
                    };

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
