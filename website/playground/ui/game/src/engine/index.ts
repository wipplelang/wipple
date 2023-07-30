export * as inputs from "./inputs";
export * as backends from "./backends";

export type Room = (ctx: GameContext) => Promise<{ shouldContinue: boolean }>;

export interface GameInput {
    button: () => Promise<number>;
}

export interface GameBackend {
    width: number;
    height: number;
    render: (game: RenderedGame) => Promise<void>;
}

export interface RenderedGame {
    text: Character[];
    music: Music | null;
}

export class Character {
    public static empty = new Character(0, "white", "black");

    public constructor(public glyph: number, public fg: string, public bg: string) {}
}

export class Music {
    public constructor(public song: string, public loop: boolean) {}

    public equals(other: Music | null): boolean {
        if (!other) return false;

        return this.song === other.song && this.loop === other.loop;
    }
}

export class GameContext {
    private input: GameInput;
    private backend: GameBackend;
    private renderedGame: RenderedGame;
    private room: Room;
    private x: number;
    private y: number;
    private fgColor: string;
    private bgColor: string;
    private stopFlag: boolean;

    public constructor(room: Room, input: GameInput, backend: GameBackend) {
        this.input = input;
        this.backend = backend;
        this.renderedGame = {
            text: new Array<Character>(backend.width * backend.height).fill(Character.empty),
            music: null,
        };
        this.room = room;
        this.x = 0;
        this.y = 0;
        this.fgColor = "white";
        this.bgColor = "black";
        this.stopFlag = false;
    }

    public setRoom(room: Room) {
        this.room = room;
    }

    public fg(color: string) {
        this.fgColor = color;
    }

    public bg(color: string) {
        this.bgColor = color;
    }

    public print(s: string | number | (string | number)[] = [], newline = true) {
        if (typeof s === "string") {
            s = [...s];
        } else if (typeof s === "number") {
            s = [s];
        }

        for (let c of s) {
            switch (typeof c) {
                case "string":
                    if (c.length !== 1) {
                        throw new Error("invalid character");
                    }

                    c = c.charCodeAt(0);
                    break;
                case "number":
                    break;
                default:
                    throw new Error("invalid character; expected string or number");
            }

            this.printGlyph(c);
        }

        if (newline) {
            this.x = 0;
            this.y++;
        }
    }

    public printf(s: string, replacement: string | number, newline = true) {
        this.print(
            [...s].map((c) => (c === " " ? " " : replacement)),
            newline
        );
    }

    public locate(x: number, y: number) {
        this.x = x;
        this.y = y;
    }

    public printGlyph(glyph: string | number) {
        switch (typeof glyph) {
            case "string":
                if (glyph.length !== 1) {
                    throw new Error("invalid character");
                }

                glyph = glyph.charCodeAt(0);
                break;
            case "number":
                break;
            default:
                throw new Error("invalid character; expected string or number");
        }

        const character = new Character(glyph, this.fgColor, this.bgColor);
        this.renderedGame.text[this.y * this.backend.width + this.x] = character;
        this.x++;
    }

    public clear() {
        this.renderedGame.text = new Array<Character>(
            this.backend.width * this.backend.height
        ).fill(Character.empty);

        this.locate(0, 0);
    }

    public play(music: Music) {
        this.renderedGame.music = music;
    }

    public pause() {
        this.renderedGame.music = null;
    }

    public button(): Promise<number> {
        return this.input.button();
    }

    public async run() {
        const run = async () => {
            if (this.stopFlag) return;

            this.x = 0;
            this.y = 0;
            this.fgColor = "white";
            this.bgColor = "black";

            const { shouldContinue } = await this.room(this);
            if (!shouldContinue) {
                return;
            }

            await this.backend.render(this.renderedGame);

            this.clear();
            this.pause();

            const fps = 10;
            setTimeout(run, 1000 / fps);
        };

        run();
    }
}

export const run = (room: Room, input: GameInput, backend: GameBackend) =>
    new GameContext(room, input, backend).run();
