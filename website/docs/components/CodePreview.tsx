import { useEffect, useMemo } from "react";
import hljs from "highlight.js";
import PlayArrowIcon from "@mui/icons-material/PlayArrowRounded";

export interface Props {
    children: {
        props: {
            className: string;
            children: string;
        };
    };
}

const CodePreview = (props: Props) => {
    useEffect(() => {
        hljs.initHighlighting();
    }, []);

    const code = props.children.props.children;

    const playgroundLink = useMemo(() => {
        const url = new URL("https://playground.wipple.gramer.dev");
        url.searchParams.append("code", code);
        return url.toString();
    }, [code]);

    return (
        <div>
            <pre
                className={`${props.children.props.className} mb-0 bg-inherit border-gray-200`}
                style={{ borderWidth: 1 }}
            >
                <code>{code}</code>
            </pre>

            <p className="mx-3 my-0 text-right">
                <a
                    target="_blank"
                    href={playgroundLink}
                    className="no-underline text-gray-600 text-sm"
                >
                    <PlayArrowIcon className="h-5 -mt-0.5" />
                    Run in Playground
                </a>
            </p>
        </div>
    );
};

export default CodePreview;
