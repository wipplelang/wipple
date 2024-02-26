export interface Diagnostic {
    error: boolean;
    group: {
        name: string;
        explanation: string;
        example: string;
    };
    primaryLabel: Label;
    secondaryLabels: Label[];
    help: string;
    fix: Fix | null;
}

export interface Label {
    path: string;
    visiblePath: string;
    span: { start: number; end: number };
    message: string;
}

export interface Fix {
    message: string;
    before: string | null;
    replacement: string | null;
    after: string | null;
}

export type Output =
    | { type: "text"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (value: string) => Promise<boolean> }
    | {
          type: "choice";
          prompt: string;
          choices: string[];
          onSubmit: (choice: number) => Promise<void>;
      };
