import { Turtle } from "./turtle";

export interface Runtime {
    initialize: () => Promise<void>;
    onMessage: (message: string, value: any) => Promise<any>;
    cleanup: () => Promise<void>;
}

export type RuntimeComponent = React.ForwardRefExoticComponent<
    { id: string } & React.RefAttributes<Runtime>
>;

export const runtimes = {
    turtle: Turtle,
};
