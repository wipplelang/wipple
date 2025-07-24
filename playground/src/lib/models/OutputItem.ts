// display, prompt, choice

export interface DisplayOutputItem {
    type: "display";
    value: string;
}

export interface PromptOutputItem {
    type: "prompt";
    prompt: string;
    submit: (value: string) => Promise<boolean>;
}

export type OutputItem = DisplayOutputItem | PromptOutputItem;
