export interface Runtime {
    reset: () => Promise<void>;
    onMessage: (message: string, value: any) => Promise<any>;
}

export type RuntimeComponent = React.ForwardRefExoticComponent<
    { id: string } & React.RefAttributes<Runtime>
>;

export * from "./turtle";
