import * as engine from "./engine";

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = async (id: string, container: HTMLElement) => {
    const messages: [message: string, value: any][] = [];

    const scene = async (ctx: engine.GameContext) => {
        while (true) {
            const queuedMessage = messages.shift();
            if (!queuedMessage) {
                return;
            }

            const [message, value] = queuedMessage;

            try {
                switch (message) {
                    case "locate": {
                        const [x, y] = value;
                        ctx.locate(x, y);
                    }
                    case "render-glyph": {
                        const [c] = value;
                        ctx.printGlyph(c);
                        break;
                    }
                    case "commit":
                        return;
                    default:
                        throw new Error("unknown message");
                }
            } catch (error) {
                console.error("[game] error:", error);
            }

            await new Promise((resolve) => setTimeout(resolve, 0));
        }
    };

    const input = engine.inputs.combined(engine.inputs.gamepad(), engine.inputs.keyboard());
    engine.backends.ptc.run(scene, input, container);

    onMessage[id] = async (message, value) => {
        messages.push([message, value]);
    };
};
