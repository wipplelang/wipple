import { useEffect, useState } from "react";
import {
    CartesianGrid,
    Line,
    LineChart,
    ResponsiveContainer,
    Tooltip,
    XAxis,
    YAxis,
} from "recharts";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<void>) => void;
}

export const App = (props: AppProps) => {
    const [minX, setMinX] = useState(-10);
    const [maxX, setMaxX] = useState(10);
    const [minY, setMinY] = useState(-10);
    const [maxY, setMaxY] = useState(10);
    const [resolution, setResolution] = useState(0.1);
    const [func, setFunc] = useState<(x: number) => Promise<number>>();
    const [color, setColor] = useState("black");
    const [resolve, setResolve] = useState(() => () => {});

    useEffect(() => {
        (async () => {
            props.setOnMessage(
                (message, value) =>
                    new Promise((resolve) => {
                        setResolve(() => resolve);

                        try {
                            switch (message) {
                                case "min-x":
                                    setMinX(value);
                                    break;
                                case "max-x":
                                    setMaxX(value);
                                    break;
                                case "min-y":
                                    setMinY(value);
                                    break;
                                case "max-y":
                                    setMaxY(value);
                                    break;
                                case "resolution":
                                    setResolution(value);
                                    break;
                                case "function":
                                    setFunc(() => value);
                                    break;
                                case "color":
                                    setColor(value);
                                    break;
                                default:
                                    throw new Error("unknown message");
                            }
                        } catch (error) {
                            console.error("[graphing] error:", error);
                        }
                    })
            );
        })();
    }, []);

    const [data, setData] = useState<[number, number][]>([]);

    useEffect(() => {
        (async () => {
            if (!func) {
                resolve();
                return;
            }

            setData([]);
            for (let x = minX - resolution; x <= maxX + resolution; x += resolution) {
                const y = await func(x);
                setData((data) => [...data, [x, y]]);
            }

            resolve();
        })();
    }, [minX, maxX, resolution, func, resolve]);

    return (
        <div style={{ backgroundColor: "white" }}>
            <ResponsiveContainer width="100%" aspect={4 / 3}>
                <LineChart data={data}>
                    <XAxis type="number" domain={[minX, maxX]} dataKey={0} allowDataOverflow />
                    <YAxis type="number" domain={[minY, maxY]} allowDataOverflow />
                    <CartesianGrid />

                    <Line
                        dataKey={1}
                        stroke={color}
                        strokeWidth={2}
                        dot={false}
                        isAnimationActive={false}
                    />
                </LineChart>
            </ResponsiveContainer>
        </div>
    );
};
