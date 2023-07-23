import { useEffect, useState } from "react";
import { produce } from "immer";

export interface AppProps {
    setOnMessage: (handler: (message: string, value: any) => Promise<any>) => void;
}

export const App = (props: AppProps) => {
    return <p>TODO</p>;
};
