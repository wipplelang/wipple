declare global {
    interface Window {
        electron?: {
            wipple: {
                energyUsage: {
                    beginMeasuring: (id: string) => Promise<void>;
                    endMeasuring: (id: string) => Promise<number | undefined>;
                };
            };
        };
    }
}

export {};
