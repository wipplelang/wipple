import { useEffect, useRef } from "react";
import turtley from "./turtley";
import turtleImage from "./assets/turtle.png";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

const defaultWidth = 200;
const defaultHeight = 200;

export const App = (props: AppProps) => {
    const containerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        (async () => {
            const turtleImageElement = document.createElement("img");
            turtleImageElement.src = turtleImage;

            const turtle = new turtley.Turtle();
            turtle.init({
                appendTo: containerRef.current,
                width: defaultWidth,
                height: defaultHeight,
                turtleImage: turtleImageElement,
                turtleWidth: 30,
                turtleHeight: 30,
            });

            await turtle.setSpeed(Infinity);
            await turtle.rotateLeft(90);
            await turtle.setPenSize(2);
            await turtle.setBackgroundColor("white");
            await turtle.setPenColor("black");
            await turtle.penDown();
            await turtle.setSpeed(0.65);

            let x = 0;
            let y = 0;
            props.setOnMessage(async (message, value) => {
                try {
                    switch (message) {
                        case "width":
                            turtle._width = value;
                            turtle.resize();
                            await new Promise(requestAnimationFrame);
                            break;
                        case "height":
                            turtle._height = value;
                            turtle.resize();
                            await new Promise(requestAnimationFrame);
                            break;
                        case "background":
                            turtle.setBackgroundColor(value);
                            break;
                        case "speed":
                            turtle.setSpeed(value);
                            break;
                        case "position-x":
                            x = value;
                            await turtle.penUp();
                            await turtle.moveTo(x, y);
                            await turtle.penDown();
                            break;
                        case "position-y":
                            y = value;
                            await turtle.penUp();
                            await turtle.moveTo(x, y);
                            await turtle.penDown();
                            break;
                        case "pen-up":
                            await turtle.penUp();
                            break;
                        case "pen-down":
                            await turtle.penDown();
                            break;
                        case "color":
                            await turtle.setPenColor(value);
                            break;
                        case "fill":
                            await turtle.setFillColor(value);
                            await turtle.fillShape();
                            break;
                        case "forward":
                            await turtle.forward(value);
                            break;
                        case "backward":
                            await turtle.backward(value);
                            break;
                        case "left":
                            await turtle.rotateLeft(value);
                            break;
                        case "right":
                            await turtle.rotateRight(value);
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

    return <div ref={containerRef} />;
};
