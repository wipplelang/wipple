import styleInject from "style-inject";
import * as engine from "./engine";
import css from "./assets/global.css?inline";

styleInject(css);

export const onMessage: Record<string, (message: string, value: any) => Promise<any>> = {};

export const initialize = async (id: string, container: HTMLElement) => {
    let ctx: engine.GameContext;
    let commit: (flags: { shouldContinue: boolean }) => Promise<void>;

    const scene = (
        gameCtx: engine.GameContext,
        gameCommit: (flags: { shouldContinue: boolean }) => Promise<void>
    ) => {
        ctx = gameCtx;
        commit = gameCommit;
    };

    onMessage[id] = async (message, value) => {
        try {
            switch (message) {
                case "locate": {
                    const [x, y] = value;
                    ctx.locate(x, y);
                    return null;
                }
                case "fg": {
                    ctx.fg(value);
                    return null;
                }
                case "bg": {
                    ctx.bg(value);
                    return null;
                }
                case "render-glyph": {
                    ctx.printGlyph(value);
                    return null;
                }
                case "render-line": {
                    ctx.print(value);
                    return null;
                }
                case "render": {
                    ctx.print(value, false);
                    return null;
                }
                case "render-line-fill": {
                    const [string, replacement] = value;
                    ctx.printf(string, replacement);
                    return null;
                }
                case "render-fill": {
                    const [string, replacement] = value;
                    ctx.printf(string, replacement, false);
                    return null;
                }
                case "play": {
                    const [song, loop] = value;
                    ctx.play(new engine.Music(song, loop === 1));
                    return null;
                }
                case "pause": {
                    ctx.pause();
                    return null;
                }
                case "button": {
                    const button = await ctx.button();
                    return button;
                }
                case "commit":
                    await commit({ shouldContinue: true });
                    return null;
                case "stop":
                    await commit({ shouldContinue: false });
                    return null;
                default:
                    throw new Error("unknown message");
            }
        } catch (error) {
            console.error("[game] error:", error);
        }
    };

    const element = document.createElement("div");
    element.id = "game";
    container.appendChild(element);

    const input = engine.inputs.combined(engine.inputs.gamepad(), engine.inputs.keyboard());
    engine.backends.ptc.run(scene, input, element);
};

export const cleanup = async (id: string) => {
    await onMessage[id]("stop", null);
    delete onMessage[id];
};
