// display, prompt, choice

export interface DisplayOutputItem {
    type: "display";
    value: string;
}

export interface PromptOutputItem {
    type: "prompt";
    prompt: string;
    onsubmit?: (value: string) => void;
    valid: boolean;
}

export type OutputItem = DisplayOutputItem | PromptOutputItem;
