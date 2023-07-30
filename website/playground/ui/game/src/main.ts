import styleInject from "style-inject";
import * as engine from "./engine";
import css from "./assets/global.css?inline";

styleInject(css);

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = async (id: string, container: HTMLElement) => {
    const messages: [message: string, value: any, callback: () => void][] = [];

    const scene = async (ctx: engine.GameContext) => {
        while (true) {
            const queuedMessage = messages.shift();
            if (!queuedMessage) {
                await new Promise((resolve) => setTimeout(resolve));
                continue;
            }

            const [message, value, callback] = queuedMessage;

            try {
                switch (message) {
                    case "locate": {
                        const [x, y] = value;
                        ctx.locate(x, y);
                        callback();
                        break;
                    }
                    case "fg": {
                        ctx.fg(value);
                        callback();
                        break;
                    }
                    case "bg": {
                        ctx.bg(value);
                        callback();
                        break;
                    }
                    case "render-glyph": {
                        ctx.printGlyph(value);
                        callback();
                        break;
                    }
                    case "render-line": {
                        ctx.print(value);
                        callback();
                        break;
                    }
                    case "render": {
                        ctx.print(value, false);
                        callback();
                        break;
                    }
                    case "commit":
                        callback();
                        return { shouldContinue: true };
                    case "stop":
                        callback();
                        return { shouldContinue: false };
                    default:
                        throw new Error("unknown message");
                }
            } catch (error) {
                console.error("[game] error:", error);
            }
        }
    };

    const element = document.createElement("div");
    element.id = "game";
    container.appendChild(element);

    const input = engine.inputs.combined(engine.inputs.gamepad(), engine.inputs.keyboard());
    engine.backends.ptc.run(scene, input, element);

    onMessage[id] = (message, value) => {
        return new Promise<void>((resolve) => {
            messages.push([message, value, resolve]);
        });
    };
};

export const cleanup = async (id: string) => {
    await onMessage[id]("stop", null);
    delete onMessage[id];
};
