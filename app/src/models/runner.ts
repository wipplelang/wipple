export type Output =
    | { type: "text"; text: string }
    | { type: "prompt"; prompt: string; onSubmit: (value: string) => Promise<boolean> }
    | {
          type: "choice";
          prompt: string;
          choices: string[];
          onSubmit: (choice: number) => Promise<void>;
      }
    | { type: "error"; message: string };
