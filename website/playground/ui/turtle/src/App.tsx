import { useEffect, useRef } from "react";
import Turtle from "real-turtle";
import turtleIcon from "./assets/turtle.png";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<void>) => void;
}

export const App = (props: AppProps) => {
    const turtle = useRef<any>(null);
    const containerRef = useRef<HTMLDivElement>(null);
    const canvasRef = useRef<HTMLCanvasElement>(null);

    useEffect(() => {
        (async () => {
            turtle.current = new Turtle(canvasRef.current, {
                async: true,
                image: turtleIcon,
                state: {
                    size: 30,
                },
            });

            await new Promise(requestAnimationFrame);

            await turtle.current.setSpeed(0.65);
            await turtle.current.setLineWidth(2);

            let x = 0;
            let y = 0;
            props.setOnMessage(async (message, value) => {
                try {
                    switch (message) {
                        case "width":
                            canvasRef.current!.width = value;
                            break;
                        case "height":
                            canvasRef.current!.height = value;
                            break;
                        case "background":
                            canvasRef.current!.style.background = value;
                            break;
                        case "position-x":
                            x = value;
                            await turtle.current.setPosition(x, y);
                            break;
                        case "position-y":
                            y = value;
                            await turtle.current.setPosition(x, y);
                            break;
                        case "pen-up":
                            await turtle.current.penUp();
                            break;
                        case "pen-down":
                            await turtle.current.penDown();
                            break;
                        case "color":
                            await turtle.current.setStrokeStyle(value);
                            break;
                        case "fill":
                            await turtle.current.setFillStyle(value);
                            await turtle.current.fill();
                            break;
                        case "begin-path":
                            await turtle.current.beginPath();
                            break;
                        case "close-path":
                            await turtle.current.closePath();
                            break;
                        case "forward":
                            await turtle.current.forward(value);
                            break;
                        case "backward":
                            await turtle.current.back(value);
                            break;
                        case "left":
                            await turtle.current.left(value);
                            break;
                        case "right":
                            await turtle.current.right(value);
                            break;
                        default:
                            throw new Error("unknown message");
                    }
                } catch (error) {
                    console.error("[turtle] error:", error);
                }
            });
        })();
    }, []);

    return (
        <div ref={containerRef}>
            <canvas ref={canvasRef} />
        </div>
    );
};
