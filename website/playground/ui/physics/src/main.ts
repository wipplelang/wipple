import matter from "matter-js";

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = async (id: string, container: HTMLElement) => {
    const element = document.createElement("div");
    container.appendChild(element);

    const engine = matter.Engine.create({
        gravity: { y: 0 }, // let the user define gravity!
    });

    const render = matter.Render.create({ element, engine });
    matter.Render.run(render);

    const runner = matter.Runner.create();
    matter.Runner.run(runner, engine);

    const bodies: matter.Body[] = [];

    onMessage[id] = async (message, value) => {
        try {
            switch (message) {
                case "run": {
                    const run = () => {
                        const start = new Date();
                        requestAnimationFrame(() => {
                            if (value((new Date().valueOf() - start.valueOf()) / 1000)) {
                                run();
                            }
                        });
                    };

                    break;
                }
                case "create-rectangle": {
                    const [x, y, width, height] = value;

                    const rectangle = matter.Bodies.rectangle(x, y, width, height);
                    matter.Composite.add(engine.world, [rectangle]);

                    const bodyIndex = bodies.length;
                    bodies.push(rectangle);

                    return bodyIndex;
                }
                case "set-position": {
                    const [bodyIndex, x, y] = value;

                    const body = bodies[bodyIndex];
                    matter.Body.setPosition(body, { x, y });

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
