import type { WithInfo, main } from "wipple-compiler";
import { LinesAndColumns, SourceLocation } from "lines-and-columns";

export type AnyDeclaration =
    | { type: "type"; path: main.Path; declaration: main.TypeDeclaration }
    | { type: "trait"; path: main.Path; declaration: main.TraitDeclaration }
    | { type: "typeParameter"; path: main.Path; declaration: main.TypeParameterDeclaration }
    | { type: "constant"; path: main.Path; declaration: main.ConstantDeclaration }
    | { type: "instance"; path: main.Path; declaration: main.InstanceDeclaration };

export interface RenderedSourceLocation {
    path: string;
    visiblePath: string;
    start: SourceLocation & { index: number };
    end: SourceLocation & { index: number };
}

export interface RenderedDiagnostic {
    location: RenderedSourceLocation;
    severity: "warning" | "error";
    message: string;
    fix: RenderedFix | null;
}

export interface RenderedFix {
    // TODO
}

export interface RenderedDocumentation {
    docs: string;
    attributes: RenderedDocumentationAttribute[];
}

export interface RenderedDocumentationAttribute {
    name: string;
    value: RenderedDocumentationAttributeValue;
}

export type RenderedDocumentationAttributeValue =
    | { type: "string"; value: string }
    | { type: "formattedString"; segments: [string, string][]; trailing: string }
    | { type: "boolean"; value: boolean };

export class Render {
    private files: Record<string, main.File & { linesAndColumns: LinesAndColumns }> = {};
    private declarations: WithInfo<main.Info, AnyDeclaration>[] = [];
    private libraries: main.UnlinkedLibrary[] = [];

    updateFiles(files: main.File[]) {
        for (const file of files) {
            this.files[file.path] = { ...file, linesAndColumns: new LinesAndColumns(file.code) };
        }
    }

    updateInterface(interface_: main.Interface) {
        const declarations: WithInfo<main.Info, AnyDeclaration>[] = [];
        for (const type of ["type", "trait", "typeParameter", "constant", "instance"] as const) {
            for (const [path, declaration] of Object.entries<WithInfo<main.Info, any>>(
                interface_[`${type}Declarations`],
            )) {
                declarations.push({
                    info: declaration.info,
                    item: {
                        path,
                        type,
                        declaration: declaration.item,
                    },
                });
            }
        }

        this.declarations = declarations;
    }

    updateLibraries(libraries: main.UnlinkedLibrary[]) {
        this.libraries = libraries;
    }

    getDeclarationFromPath(path: main.Path): WithInfo<main.Info, AnyDeclaration> | null {
        for (const declaration of this.declarations) {
            if (declaration.item.path === path) {
                return declaration;
            }
        }

        return null;
    }

    getInfoAtCursor(path: string, index: number): main.Info | null {
        for (const item of this.libraries.flatMap((library) => [
            ...Object.values(library.items),
            ...library.code,
        ])) {
            if (
                item.expression.info.location.path !== "top-level" &&
                (path !== item.expression.info.location.path ||
                    index < item.expression.info.location.span.start ||
                    index > item.expression.info.location.span.end)
            ) {
                continue;
            }

            const candidates: WithInfo<main.Info, main.TypedExpression>[] = [];

            this.traverseExpression(item.expression, (expression) => {
                if (
                    index >= expression.info.location.span.start &&
                    index <= expression.info.location.span.end
                ) {
                    candidates.push(expression);
                }

                return false;
            });

            if (candidates.length === 0) {
                continue;
            }

            candidates.sort((left, right) => {
                const length = (expression: WithInfo<main.Info, main.TypedExpression>) =>
                    expression.info.location.span.end - expression.info.location.span.start;

                return length(left) - length(right);
            });

            const expression = candidates[0];
            switch (expression.item.kind.type) {
                case "constant":
                case "trait":
                    return (
                        this.getDeclarationFromPath(expression.item.kind.value.path)?.info ?? null
                    );
                case "variable":
                    return this.getDeclarationFromPath(expression.item.kind.value[1])?.info ?? null;
                default:
                    return expression.info;
            }
        }

        return null;
    }

    renderSourceLocation(value: WithInfo<main.Info, unknown>): RenderedSourceLocation | null {
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
            start: { ...startLocation, index: value.info.location.span.start },
            end: { ...endLocation, index: value.info.location.span.end },
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
            fix: null,
        };
    }

    renderDocumentation(value: WithInfo<main.Info, unknown>): RenderedDocumentation | null {
        const renderedSourceLocation = this.renderSourceLocation(value);
        if (!renderedSourceLocation) {
            return null;
        }

        const lines = this.files[renderedSourceLocation.path].code.split("\n");

        let line = renderedSourceLocation.start.line - 1;
        if (line < 0) {
            return null;
        }

        const docLines: string[] = [];
        while (true) {
            const code = lines[line];
            if (!code) {
                break;
            }

            if (code.startsWith("--")) {
                docLines.push(code.slice(2));
                line--;
            } else {
                break;
            }
        }

        if (docLines.length === 0) {
            return null;
        }

        docLines.reverse();

        const docs: string[] = [];
        const attributes: RenderedDocumentationAttribute[] = [];

        for (let docLine of docLines) {
            docLine = docLine.trim();

            const attributeMatch = /^\[(.*)\]:(.*)/.exec(docLine);
            if (attributeMatch) {
                let [_, name, value] = attributeMatch;
                name = name.trim();
                value = value.trim();

                const stringMatch = /^"((?:[^"\\]|\\.)*)"(.*)$/.exec(value);
                if (stringMatch) {
                    let [_, string, rest] = stringMatch;

                    const values = rest.split(" ").flatMap((s) => {
                        s = s.trim();
                        return s === "" ? [] : [s];
                    });

                    if (values.length === 0) {
                        attributes.push({
                            name: name.trim(),
                            value: { type: "string", value: string },
                        });
                    } else {
                        const stringSegments = string.split("_");

                        const trailing = stringSegments.pop() ?? "";

                        const segments = stringSegments.map(
                            (s, i) => [s, values[i] ?? ""] as [string, string],
                        );

                        attributes.push({
                            name: name.trim(),
                            value: { type: "formattedString", segments, trailing },
                        });
                    }

                    continue;
                }

                if (value === "True" || value === "False") {
                    attributes.push({
                        name: name.trim(),
                        value: { type: "boolean", value: value === "True" },
                    });

                    continue;
                }
            }

            docs.push(docLine);
        }

        return { docs: docs.join("\n"), attributes };
    }

    private traverseExpression(
        expression: WithInfo<main.Info, main.TypedExpression>,
        f: (expression: WithInfo<main.Info, main.TypedExpression>) => boolean,
    ): WithInfo<main.Info, main.TypedExpression> | null {
        const traverse = (expression: WithInfo<main.Info, main.TypedExpression>) => {
            if (f(expression)) {
                throw expression;
            }

            switch (expression.item.kind.type) {
                case "function":
                    traverse(expression.item.kind.value.body);
                    break;
                case "block":
                    for (const statement of expression.item.kind.value) {
                        traverse(statement);
                    }

                    break;
                case "do":
                    traverse(expression.item.kind.value);
                    break;
                case "call":
                    traverse(expression.item.kind.value.function);

                    for (const input of expression.item.kind.value.inputs) {
                        traverse(input);
                    }

                    break;
                case "when":
                    traverse(expression.item.kind.value.input);

                    for (const arm of expression.item.kind.value.arms) {
                        traverse(arm.item.body);
                    }

                    break;
                case "intrinsic":
                    for (const input of expression.item.kind.value.inputs) {
                        traverse(input);
                    }

                    break;
                case "initialize":
                    traverse(expression.item.kind.value.value);
                    break;
                case "mutate":
                    traverse(expression.item.kind.value.value);
                    break;
                case "marker":
                    break;
                case "structure":
                    for (const field of expression.item.kind.value.fields) {
                        traverse(field.item.value);
                    }

                    break;
                case "variant":
                    for (const value of expression.item.kind.value.values) {
                        traverse(value);
                    }

                    break;
                case "wrapper":
                    traverse(expression.item.kind.value);
                    break;
                case "tuple":
                    for (const value of expression.item.kind.value) {
                        traverse(value);
                    }

                    break;
                case "format":
                    for (const segment of expression.item.kind.value.segments) {
                        traverse(segment.value);
                    }

                    break;
                case "number":
                    break;
                case "unknown":
                    break;
                case "variable":
                    break;
                case "constant":
                    break;
                case "trait":
                    break;
                case "text":
                    break;
                default:
                    expression.item.kind satisfies never;
                    break;
            }
        };

        try {
            traverse(expression);
        } catch (expression: any) {
            return expression;
        }

        return null;
    }
}
