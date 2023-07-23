export interface AnalysisOutputDiagnostic {
    level: "warning" | "error";
    message: string;
    fix?: AnalysisConsoleDiagnosticFix;
    notes: {
        code: string;
        span: {
            file: string;
            start: number;
            end: number;
        };
        messages: string[];
    }[];
}

export interface AnalysisConsoleDiagnosticFix {
    description: string;
    start: number;
    end: number;
    replacement: string;
}
