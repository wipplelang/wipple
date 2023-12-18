import matter from "matter-js";

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = async (id: string, container: HTMLElement) => {
    const element = document.createElement("div");
    container.appendChild(element);

    const width = 800;
    const height = 600;

    const engine = matter.Engine.create({
        gravity: { y: 0 }, // let the user define gravity!
    });

    const render = matter.Render.create({
        element,
        engine,
        options: {
            width,
            height,
            background: "white",
            wireframes: false,
        },
    });

    matter.Render.run(render);

    const runner = matter.Runner.create();
    matter.Runner.run(runner, engine);

    const bodies: matter.Body[] = [];

    const start = new Date();

    const flip = () => matter.Composite.scale(engine.world, 1, -1, { x: width / 2, y: height / 2 });

    onMessage[id] = async (message, value) => {
        try {
            switch (message) {
                case "clock":
                    return (new Date().valueOf() - start.valueOf()) / 1000;
                case "within-bounds":
                    return engine.world.bodies.every(
                        (body) =>
                            body.position.x >= -value &&
                            body.position.x <= width + value &&
                            body.position.y >= -value &&
                            body.position.y <= height + value
                    );
                case "create-rectangle": {
                    const [x, y, width, height] = value;

                    const rectangle = matter.Bodies.rectangle(x, y, width, height);
                    matter.Composite.add(engine.world, [rectangle]);

                    flip();

                    const bodyIndex = bodies.length;
                    bodies.push(rectangle);

                    return bodyIndex;
                }
                case "set-position": {
                    const [bodyIndex, x, y] = value;

                    const body = bodies[bodyIndex];
                    matter.Body.setPosition(body, { x, y });

                    flip();

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
