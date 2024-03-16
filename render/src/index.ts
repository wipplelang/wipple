import type { WithInfo, main } from "wipple-compiler";
import { LinesAndColumns, SourceLocation } from "lines-and-columns";

export type AnyDeclaration = { name: string | null; path: main.Path } & (
    | { type: "type"; declaration: main.TypeDeclaration }
    | { type: "trait"; declaration: main.TraitDeclaration }
    | { type: "typeParameter"; declaration: main.TypeParameterDeclaration }
    | { type: "constant"; declaration: main.ConstantDeclaration }
    | { type: "instance"; declaration: main.InstanceDeclaration }
);

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
                        name: this.nameForPath(path),
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

    getDeclarationFromInfo(info: main.Info): WithInfo<main.Info, AnyDeclaration> | null {
        for (const declaration of this.declarations) {
            if (this.compareInfo(declaration.info, info)) {
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

    renderDeclaration(declaration: WithInfo<main.Info, AnyDeclaration>): string | null {
        switch (declaration.item.type) {
            case "type": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    [],
                    { kind: "arrow" },
                );

                return `${declaration.item.name} : ${typeFunction}type`;
            }
            case "trait": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    [],
                    { kind: "arrow" },
                );

                return `${declaration.item.name} : ${typeFunction}trait`;
            }
            case "typeParameter": {
                return declaration.item.name;
            }
            case "constant": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    declaration.item.declaration.bounds,
                    { kind: "arrow" },
                );

                const type = this.renderType(declaration.item.declaration.type, true);

                return `${declaration.item.name} :: ${typeFunction}${type}`;
            }
            case "instance": {
                const typeFunction = this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    declaration.item.declaration.bounds,
                    { kind: "arrow" },
                );

                const instance = this.renderInstance(declaration.item.declaration.instance);

                return `${typeFunction}instance ${instance}`;
            }
            default:
                declaration.item satisfies never;
                return null;
        }
    }

    renderType(type: WithInfo<main.Info, main.Type>, isTopLevel: boolean): string {
        const render = (
            type: WithInfo<main.Info, main.Type>,
            isTopLevel: boolean,
            isReturn: boolean,
        ): string => {
            switch (type.item.type) {
                case "unknown": {
                    return "_";
                }
                case "parameter": {
                    return this.nameForPath(type.item.value);
                }
                case "declared": {
                    const name = this.nameForPath(type.item.value.path);
                    if (!name) {
                        return "_";
                    }

                    const rendered =
                        type.item.value.parameters.length === 0
                            ? name
                            : `${name} ${type.item.value.parameters
                                  .map((parameter) => render(parameter, false, false))
                                  .join(" ")}`;

                    return isTopLevel || type.item.value.parameters.length === 0
                        ? rendered
                        : `(${rendered})`;
                }
                case "function": {
                    const inputs = type.item.value.inputs
                        .map((input) => render(input, false, false))
                        .join(" ");

                    const output = render(type.item.value.output, false, true);

                    const rendered = `${inputs} -> ${output}`;

                    return isTopLevel && isReturn ? rendered : `(${rendered})`;
                }
                case "tuple": {
                    const rendered =
                        type.item.value.length === 0
                            ? "()"
                            : type.item.value.length === 1
                            ? `${render(type.item.value[0], isTopLevel, isReturn)} ;`
                            : `(${type.item.value
                                  .map((value) => render(value, false, false))
                                  .join(" ; ")})`;

                    return isTopLevel || type.item.value.length === 0 ? rendered : `(${rendered})`;
                }
                case "block": {
                    return `{${render(type.item.value, true, false)}}`;
                }
                case "intrinsic": {
                    return "intrinsic";
                }
            }
        };

        return render(type, isTopLevel, true);
    }

    renderTypeFunction(
        parameters: main.Path[],
        bounds: WithInfo<main.Info, main.Instance>[],
        format: { kind: "arrow" } | { kind: "description"; type: WithInfo<main.Info, main.Type> },
    ): string {
        switch (format.kind) {
            case "arrow": {
                if (parameters.length === 0) {
                    return "";
                }

                const renderedParameters = parameters
                    .map((parameter) => this.nameForPath(parameter))
                    .join(" ");

                let renderedBounds = bounds
                    .map((bound) => `${this.renderInstance(bound)}`)
                    .join(" ");

                if (renderedBounds) {
                    renderedBounds = ` where ${renderedBounds}`;
                }

                return `${renderedParameters}${renderedBounds} => `;
            }
            case "description": {
                const renderedType = this.renderType(format.type, true);

                switch (parameters.length) {
                    case 0: {
                        return `\`${renderedType}\``;
                    }
                    case 1: {
                        return `\`${renderedType}\` for any type \`${this.nameForPath(
                            parameters[0],
                        )}\``;
                    }
                    default: {
                        const last = parameters.pop()!;

                        return `\`${this.renderType(format.type, true)}\` for any types ${parameters
                            .map((parameter) => `\`${this.nameForPath(parameter)}\``)
                            .join(", ")} and \`${this.nameForPath(last)}\``;
                    }
                }
            }
            default:
                format satisfies never;
                throw new Error("unknown format");
        }
    }

    renderInstance(instance: WithInfo<main.Info, main.Instance>): string {
        const trait = this.nameForPath(instance.item.trait);

        const parameters = instance.item.parameters
            .map((type) => this.renderType(type, false))
            .join(" ");

        return parameters.length === 0 ? trait : `(${trait} ${parameters})`;
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

    private compareInfo(left: main.Info, right: main.Info): boolean {
        return (
            left.location.path === right.location.path &&
            left.location.span.start === right.location.span.start &&
            left.location.span.end === right.location.span.end
        );
    }

    private nameForPath(path: main.Path): string {
        return path.split(" / ").pop()?.split(" ").pop() ?? "<unknown>";
    }
}
