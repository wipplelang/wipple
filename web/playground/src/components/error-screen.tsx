import { MaterialSymbol } from "react-material-symbols";
import { Button } from "./button";

export const ErrorScreen = (props: { error: Error }) => (
    <div className="relative w-screen h-screen flex flex-col items-center justify-center gap-4">
        <div className="flex flex-col items-center text-center gap-2">
            <MaterialSymbol icon="error" size={40} />
            <p>
                Wipple encountered an internal problem.
                <br />
                Please reload the page — your code will be saved.
            </p>
        </div>

        <Button role="primary" icon="refresh" fill onClick={() => window.location.reload()}>
            Reload
        </Button>

        <div className="absolute inset-x-4 bottom-4 text-xs opacity-50 text-center">
            {props.error.toString()}
        </div>
    </div>
);
