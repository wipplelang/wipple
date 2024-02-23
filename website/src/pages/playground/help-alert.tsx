import { Button, Markdown } from "../../components";
import { Help } from "../../models";

export const HelpAlert = (props: { help: Help; dismiss: () => void }) => (
    <div className="w-[512px] help">
        <div className="flex flex-col gap-4 w-full">
            <div className="flex flex-col gap-2">
                <h1 className="text-2xl">
                    <code>{props.help.name}</code>
                </h1>

                <h2 className="text-gray-500 text-lg">
                    <Markdown>{props.help.summary}</Markdown>
                </h2>
            </div>

            <Markdown>{props.help.doc || "No additional documentation."}</Markdown>

            <Button role="primary" fill onClick={props.dismiss}>
                Done
            </Button>
        </div>
    </div>
);
