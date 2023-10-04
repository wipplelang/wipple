import * as fastiles from "fastiles";
import { GameBackend, GameInput, Music, Scene, run as runGame } from "../../index";
import font from "./assets/font.png";

export const width = 32;
export const height = 24;

export const run = async (scene: Scene, input: GameInput, element: HTMLElement) => {
    const palette = new fastiles.Palette();
    const cache: Record<string, number> = {};

    const color = (color: string) => {
        if (cache[color] != null) {
            return cache[color];
        }

        const index = palette.add(color);
        cache[color] = index;
        return index;
    };

    const fontImage = await new Promise<HTMLImageElement>((resolve, reject) => {
        const image = new Image();
        image.onload = () => resolve(image);
        image.onerror = reject;
        image.src = font;
    });

    const canvas = new fastiles.Scene({
        tileSize: [8, 8],
        tileCount: [width, height],
        font: fontImage,
    });

    canvas.palette = palette;

    canvas.node.style.visibility = "hidden";
    element.appendChild(canvas.node);

    let audio: HTMLAudioElement | null = null;
    let prevMusic: Music | null = null;

    const backend: GameBackend = {
        width,
        height,
        render: async ({ text, music }) => {
            canvas.node.style.visibility = "visible";

            for (let y = 0; y < height; y++) {
                for (let x = 0; x < width; x++) {
                    const character = text[y * width + x];
                    const fg = color(character.fg);
                    const bg = color(character.bg);
                    canvas.draw([x, y], character.glyph, fg, bg);
                }
            }

            if (music) {
                if (!music.equals(prevMusic)) {
                    audio?.pause();

                    audio = new Audio(music.song);
                    audio.loop = music.loop;
                    audio.play();
                }
            } else {
                audio?.pause();
                audio = null;
            }

            prevMusic = music;
        },
    };

    runGame(scene, input, backend);
};
