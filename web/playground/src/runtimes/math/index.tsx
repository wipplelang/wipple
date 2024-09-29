import type { RuntimeComponent } from "..";
import { forwardRef, useEffect, useImperativeHandle, useState } from "react";
import { CartesianGrid, Line, LineChart, ResponsiveContainer, XAxis, YAxis } from "recharts";
import { PaletteCategory } from "../../models";
import { produce } from "immer";

const defaultMinX = -10;
const defaultMaxX = 10;
const defaultMinY = -10;
const defaultMaxY = 10;
const defaultResolution = 0.25;

export const Math: RuntimeComponent = forwardRef((props, ref) => {
    const [minX, setMinX] = useState(defaultMinX);
    const [maxX, setMaxX] = useState(defaultMaxX);
    const [minY, setMinY] = useState(defaultMinY);
    const [maxY, setMaxY] = useState(defaultMaxY);
    const [resolution, setResolution] = useState(defaultResolution);
    const [func, setFunc] = useState<(x: number) => Promise<number>>();
    const [colors, setColors] = useState<string[]>([]);
    const [resolve, setResolve] = useState(() => () => {});

    const [data, setData] = useState<[number, number[]][]>([]);

    useEffect(() => {
        (async () => {
            if (!func) {
                resolve();
                return;
            }

            for (let x = minX - resolution; x <= maxX + resolution; x += resolution) {
                const y = await func(x);
                setData((data) =>
                    produce(data, (data) => {
                        let ys: [number, number[]] | undefined = data.find(
                            ([dataX]) => dataX === x,
                        );

                        if (!ys) {
                            ys = [x, []];
                            data.push(ys);
                        }

                        ys[1].push(y);
                    }),
                );
            }

            setFunc(undefined);

            resolve();
        })();
    }, [minX, maxX, resolution, func, resolve]);

    useImperativeHandle(ref, () => ({
        initialize: async () => {
            resolve();
            setMinX(defaultMinX);
            setMaxX(defaultMaxX);
            setMinY(defaultMinY);
            setMaxY(defaultMaxY);
            setResolution(defaultResolution);
            setFunc(undefined);
            setColors([]);
            setData([]);
        },
        onMessage: (message, value) =>
            new Promise((resolve) => {
                setResolve(() => resolve);

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
                        setFunc(
                            () =>
                                (...inputs: any[]) =>
                                    value(inputs),
                        );
                        break;
                    case "color":
                        setColors((colors) => [...colors, value]);
                        break;
                    default:
                        throw new Error(`unsupported message: ${message}`);
                }
            }),
        cleanup: async () => {
            resolve();
        },
    }));

    return (
        <div className="relative rounded-md overflow-hidden border border-gray-100 dark:border-gray-800 w-full h-full">
            <div style={{ backgroundColor: "white" }}>
                <ResponsiveContainer width="100%" aspect={4 / 3}>
                    <LineChart data={data}>
                        <XAxis type="number" domain={[minX, maxX]} dataKey={0} allowDataOverflow />

                        <YAxis type="number" domain={[minY, maxY]} allowDataOverflow />

                        <CartesianGrid />

                        {data.map((_, index) => (
                            <Line
                                key={index}
                                dataKey={(data) => data[1][index]}
                                stroke={colors[index] ?? colors[colors.length - 1] ?? "black"}
                                strokeWidth={2}
                                dot={false}
                                isAnimationActive={false}
                            />
                        ))}
                    </LineChart>
                </ResponsiveContainer>
            </div>
        </div>
    );
});

export const paletteCategories: PaletteCategory[] = [
    {
        title: "Commands",
        items: [
            {
                title: "plot",
                code: `plot (x -> x + 1)`,
            },
            {
                title: "color",
                code: `color [Color "#3b82f6"]`,
            },
        ],
    },
];
