import { AnsiUp } from "ansi_up";
import { useMemo } from "react";
import "./style.css";

export const Ansi = (props: { children: string }) => {
    const html = useMemo(() => {
        const ansiUp = new AnsiUp();
        ansiUp.use_classes = true;
        return ansiUp.ansi_to_html(props.children);
    }, [props.children]);

    return (
        <pre className="text-wrap">
            <code className="ansi" dangerouslySetInnerHTML={{ __html: html }} />
        </pre>
    );
};
