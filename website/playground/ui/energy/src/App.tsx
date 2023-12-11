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
const joulesToKilowattHours = (joules: number) => joules / (3.6 * 10 ** 6);

const averageCarbonEmissionsInKilogramsPerHouseholdPerMonth = 4000; // https://css.umich.edu/publications/factsheets/sustainability-indicators/carbon-footprint-factsheet, converted from t/yr to kg/mo

const EnergySummary = (props: { energyUsage: number }) => {
    const watts = props.energyUsage;
    const kilowattHoursAtScaleForMonth = joulesToKilowattHours(watts * secondsPerMonth * scale);

    return (
        <div className="prose prose-sky dark:prose-invert">
            <p className="font-bold">
                Your program consumed {watts.toFixed(2)} W on average. If you ran it at scale (
                {scale} computers) for a month, you would consume{" "}
                {kilowattHoursAtScaleForMonth.toFixed(2)} kWh of energy.
            </p>

            <ul>
                {energySources.map(
                    ({
                        name,
                        costInDollarsPerKilowattHours,
                        carbonEmissionsInKilogramsPerKilowattHours,
                    }) => {
                        const cost = costInDollarsPerKilowattHours(kilowattHoursAtScaleForMonth);

                        const emissions = carbonEmissionsInKilogramsPerKilowattHours(
                            kilowattHoursAtScaleForMonth
                        );

                        const factor =
                            emissions / averageCarbonEmissionsInKilogramsPerHouseholdPerMonth;

                        return (
                            <li key={name}>
                                <strong>{name}:</strong> ${cost.toFixed(2)} ({emissions.toFixed(2)}{" "}
                                kg CO<sub>2</sub>, {factor.toFixed(2)}x the emissions of the average
                                household)
                            </li>
                        );
                    }
                )}
            </ul>
        </div>
    );
};

const energySources = [
    {
        name: "Coal",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const costInDollarsPerMillionBtu = 2.37; // as of 2023 (https://www.statista.com/statistics/244479/us-consumer-price-estimates-for-coal-energy/)
            return kilowattHours * millionBtuPerKilowattHour * costInDollarsPerMillionBtu;
        },
        carbonEmissionsInKilogramsPerKilowattHours: (kilowattHours: number) => {
            const kilogramsPerMillionBtu = 95.92; // https://www.eia.gov/environment/emissions/co2_vol_mass.php
            return kilowattHours * millionBtuPerKilowattHour * kilogramsPerMillionBtu;
        },
    },
    {
        name: "Natural Gas",
        costInDollarsPerKilowattHours: (kilowattHours: number) => {
            const costInDollarsPerThousandCubicFeet = 18.88; // in Massachusetts as of October 2023 (https://www.chooseenergy.com/data-center/natural-gas-rates-by-state/)
            const thousandCubicFeetPerMillionBtu = (1039 * 1000) / 1000000; // https://www.nrg.com/resources/energy-tools/energy-conversion-calculator.html
            return (
                kilowattHours *
                millionBtuPerKilowattHour *
                costInDollarsPerThousandCubicFeet *
                thousandCubicFeetPerMillionBtu
            );
        },
        carbonEmissionsInKilogramsPerKilowattHours: (kilowattHours: number) => {
            const kilogramsPerMillionBtu = 52.91; // https://www.eia.gov/environment/emissions/co2_vol_mass.php
            return kilowattHours * millionBtuPerKilowattHour * kilogramsPerMillionBtu;
        },
    },
];
