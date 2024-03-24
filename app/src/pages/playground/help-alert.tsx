import { Button, Markdown } from "../../components";
import { Help } from "../../models";

export const HelpAlert = (props: { help: Help; dismiss: () => void }) => (
    <div className="w-[512px] help">
        <div className="flex flex-col w-full">
            <div className="flex flex-col gap-2">
                <h1 className="text-2xl">
                    <code>{props.help.name}</code>
                </h1>

                <h2 className="text-gray-500 text-lg">
                    <Markdown>{props.help.summary}</Markdown>
                </h2>
            </div>

            <div className="prose dark:prose-invert prose-blue prose-sm prose-code:text-sm prose-code:text-gray-900 dark:prose-code:text-gray-100">
                <Markdown>{props.help.doc || "No additional documentation."}</Markdown>
            </div>

            <Button role="primary" fill onClick={props.dismiss}>
                Done
            </Button>
        </div>
    </div>
);
