import { useEffect, useState } from "react";

export interface AppProps {
    id: string;
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

export const App = (props: AppProps) => {
    const [running, setRunning] = useState(false);
    const [handleStart, setHandleStart] = useState<() => void>();
    const [energyUsage, setEnergyUsage] = useState<number>();

    useEffect(() => {
        (async () => {
            props.setOnMessage(async (message, value) => {
                try {
                    switch (message) {
                        case "begin":
                            if (!window.electron) {
                                break;
                            }

                            await new Promise<void>((resolve) => {
                                setHandleStart(() => async () => {
                                    setRunning(true);
                                    setEnergyUsage(undefined);

                                    await window.electron!.wipple.energyUsage.beginMeasuring(
                                        props.id
                                    );

                                    resolve();
                                });
                            });

                            break;
                        case "end":
                            if (!window.electron) {
                                break;
                            }

                            const energyUsage =
                                await window.electron.wipple.energyUsage.endMeasuring(props.id);

                            setRunning(false);
                            setEnergyUsage(energyUsage);

                            break;
                        default:
                            throw new Error("unknown message");
                    }
                } catch (error) {
                    console.error("[graphing] error:", error);
                }
            });
        })();
    }, []);

    return !window.electron ? (
        <p>Error: Please run this lesson in the Wipple Playground app.</p>
    ) : running ? (
        <p>Running...</p>
    ) : energyUsage != null ? (
        <EnergySummary energyUsage={energyUsage} />
    ) : (
        <button onClick={() => handleStart!()}>Begin Measuring</button>
    );
};

const scale = 1000;

const secondsPerMonth = 2592000;
const millionBtuPerKilowattHour = 3412 / 1000000;
const thousandCubicFeetPerMillionBtu = (1039 * 1000) / 1000000; // https://www.nrg.com/resources/energy-tools/energy-conversion-calculator.html
const joulesToKilowattHours = (joules: number) => joules / (3.6 * 10 ** 6);
const averageCarbonEmissionsInKilogramsPerHouseholdPerMonth = 4000; // https://css.umich.edu/publications/factsheets/sustainability-indicators/carbon-footprint-factsheet, converted from t/yr to kg/mo

const EnergySummary = (props: { energyUsage: number }) => {
    const watts = props.energyUsage;
    const kilowattHoursAtScaleForMonth = joulesToKilowattHours(watts * secondsPerMonth * scale);

    return (
        <div className="flex flex-col gap-6 w-full">
            <div className="flex flex-col gap-4">
                <h1 className="text-lg font-bold">Energy use report</h1>

                <div className="flex gap-2">
                    <div className="flex flex-col items-center justify-center bg-green-500 px-2.5 py-2 rounded-lg">
                        <p className="text-3xl font-bold text-white">{watts.toFixed(2)} W</p>
                    </div>

                    <div className="flex flex-col items-center justify-center bg-blue-500 px-2.5 py-2 rounded-lg text-center">
                        <p className="text-lg font-bold text-white">
                            {kilowattHoursAtScaleForMonth.toFixed(0)} kWh
                        </p>

                        <p className="text-xs text-white text-opacity-70">1000 computers/mo</p>
                    </div>
                </div>
            </div>

            <div className="flex flex-col gap-4 w-full">
                <h1 className="text-lg font-bold">Compare energy sources</h1>

                <ul className="flex flex-col gap-4 w-full">
                    {energySources.map(
                        ({
                            name,
                            image,
                            costInDollarsPerKilowattHours,
                            costSource,
                            carbonEmissionsInKilogramsPerKilowattHours,
                            emissionsSource,
                        }) => {
                            const cost = costInDollarsPerKilowattHours(
                                kilowattHoursAtScaleForMonth
                            );

                            const emissions = carbonEmissionsInKilogramsPerKilowattHours(
                                kilowattHoursAtScaleForMonth
                            );

                            const factor =
                                emissions / averageCarbonEmissionsInKilogramsPerHouseholdPerMonth;

                            return (
                                <li key={name} className="flex flex-row items-start gap-2">
                                    <img src={image} alt={name} className="w-10 h-10" />

                                    <div className="flex flex-col justify-center mt-1">
                                        <h2 className="font-bold">{name}</h2>

                                        <div className="text-sm">
                                            <p>
                                                <strong>Price:</strong> ${cost.toFixed(2)}{" "}
                                                <a
                                                    href={costSource}
                                                    target="_blank"
                                                    className="text-blue-500"
                                                >
                                                    (source)
                                                </a>
                                            </p>

                                            <p>
                                                <strong>
                                                    CO<sub>2</sub> emissions:
                                                </strong>{" "}
                                                {emissions.toFixed(2)} kg
                                                <span className="text-gray-500">
                                                    {" • "}
                                                    {factor.toFixed(2)}x the emissions of the
                                                    average US household
                                                </span>{" "}
                                                <a
                                                    href={emissionsSource}
                                                    target="_blank"
                                                    className="text-blue-500"
                                                >
                                                    (source)
                                                </a>
                                            </p>
                                        </div>
                                    </div>
                                </li>
                            );
                        }
                    )}
                </ul>
            </div>
        </div>
    );
};

const energySources = [
    {
        name: "Coal",
        image: "https://img.icons8.com/color/48/manufacturing.png",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const costInDollarsPerMillionBtu = 2.37;
            return kilowattHours * millionBtuPerKilowattHour * costInDollarsPerMillionBtu;
        },
        costSource:
            "https://www.statista.com/statistics/244479/us-consumer-price-estimates-for-coal-energy/",
        carbonEmissionsInKilogramsPerKilowattHours: (kilowattHours: number) => {
            const kilogramsPerMillionBtu = 95.92;
            return kilowattHours * millionBtuPerKilowattHour * kilogramsPerMillionBtu;
        },
        emissionsSource: "https://www.eia.gov/environment/emissions/co2_vol_mass.php",
    },
    {
        name: "Natural Gas",
        image: "https://img.icons8.com/color/96/gas-industry.png",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const costInDollarsPerThousandCubicFeet = 18.88;
            return (
                kilowattHours *
                millionBtuPerKilowattHour *
                costInDollarsPerThousandCubicFeet *
                thousandCubicFeetPerMillionBtu
            );
        },
        costSource: "https://www.chooseenergy.com/data-center/natural-gas-rates-by-state/", // Massachusetts
        carbonEmissionsInKilogramsPerKilowattHours: (kilowattHours: number) => {
            const kilogramsPerMillionBtu = 52.91;
            return kilowattHours * millionBtuPerKilowattHour * kilogramsPerMillionBtu;
        },
        emissionsSource: "https://www.eia.gov/environment/emissions/co2_vol_mass.php",
    },
];
