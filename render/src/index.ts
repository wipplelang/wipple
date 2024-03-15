import type { WithInfo, main } from "wipple-compiler";
import { LinesAndColumns, SourceLocation } from "lines-and-columns";

export interface RenderedSourceLocation {
    path: string;
    visiblePath: string;
    start: SourceLocation;
    end: SourceLocation;
}

export interface RenderedDiagnostic {
    location: RenderedSourceLocation;
    severity: "warning" | "error";
    message: string;
}

export class Render {
    private files: Record<string, main.File & { linesAndColumns: LinesAndColumns }> = {};

    updateFiles(files: main.File[]) {
        for (const file of files) {
            this.files[file.path] = { ...file, linesAndColumns: new LinesAndColumns(file.code) };
        }
    }

    renderSourceLocation(value: WithInfo<main.Info, unknown>) {
        const file = this.files[value.info.location.path];
        if (!file) {
            return null;
        }

        const startLocation = file.linesAndColumns.locationForIndex(value.info.location.span.start);
        if (!startLocation) {
            return null;
        }

        const endLocation = file.linesAndColumns.locationForIndex(value.info.location.span.end);
        if (!endLocation) {
            return null;
        }

        return {
            path: file.path,
            visiblePath: file.visiblePath,
            start: startLocation,
            end: endLocation,
        };
    }

    renderDiagnostic(diagnostic: WithInfo<main.Info, main.Diagnostic>): RenderedDiagnostic | null {
        const renderedSourceLocation = this.renderSourceLocation(diagnostic);
        if (!renderedSourceLocation) {
            return null;
        }

        // TODO
        return {
            location: renderedSourceLocation,
            severity: "error",
            message: `${diagnostic.item.type}: ${
                "value" in diagnostic.item ? JSON.stringify(diagnostic.item.value) : ""
            }`,
        };
    }
}
