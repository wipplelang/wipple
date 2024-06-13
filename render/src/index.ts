import type * as compiler from "wipple-compiler";
import { LinesAndColumns, SourceLocation } from "lines-and-columns";

export type AnyDeclaration = { name: string | null; path: compiler.lower_Path } & (
    | { type: "type"; declaration: compiler.typecheck_TypeDeclaration }
    | { type: "trait"; declaration: compiler.typecheck_TraitDeclaration }
    | { type: "typeParameter"; declaration: compiler.typecheck_TypeParameterDeclaration }
    | { type: "constant"; declaration: compiler.typecheck_ConstantDeclaration }
    | { type: "instance"; declaration: compiler.typecheck_InstanceDeclaration }
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
    message: string;
    before?: string;
    replacement?: string;
    after?: string;
}

export interface RenderedDocumentation {
    docs: string;
    example: string | null;
}

export interface RenderedDocumentationAttribute {
    name: string;
    input: string | null;
    value: RenderedDocumentationAttributeValue;
}

export type RenderedDocumentationAttributeValue =
    | { type: "string"; value: string }
    | { type: "formattedString"; segments: [string, string][]; trailing: string }
    | { type: "boolean"; value: boolean };

export interface RenderedHighlight {
    category?: string;
    icon?: string;
}

export interface RenderedSuggestion {
    kind: "type" | "trait" | "typeParameter" | "constant" | "variable" | "keyword" | "operator";
    name: string;
    code: string | null;
    docs: RenderedDocumentation | null;
}

const keywords = ["do", "when", "type", "trait", "instance", "intrinsic", "infer", "default"];
const operators = ["as", "to", "by", "is", "and", "or"];

export interface RenderConfiguration {
    describeType: (
        render: Render,
        type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
    ) => Promise<compiler.typecheck_MessageType | null>;
}

export class Render {
    private files: Record<string, compiler.File & { linesAndColumns: LinesAndColumns }> = {};
    private declarations: compiler.WithInfo<compiler.Info, AnyDeclaration>[] = [];
    public interface: compiler.Interface | null = null;
    public libraries: compiler.linker_UnlinkedLibrary[] = [];
    public ide: compiler.Ide | null = null;

    constructor(public configuration: RenderConfiguration) {}

    async update(
        interface_: compiler.Interface,
        libraries: compiler.linker_UnlinkedLibrary[],
        ide: compiler.Ide | null,
    ): Promise<void> {
        this.files = {};
        for (const file of interface_.files) {
            this.files[file.path] = { ...file, linesAndColumns: new LinesAndColumns(file.code) };
        }

        this.declarations = [];
        for (const type of ["type", "trait", "typeParameter", "constant", "instance"] as const) {
            for (const [path, declaration] of Object.entries<compiler.WithInfo<compiler.Info, any>>(
                interface_[`${type}Declarations`],
            )) {
                this.declarations.push({
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

        this.interface = interface_;
        this.libraries = libraries;
        this.ide = ide;
    }

    async getDeclarationFromPath(
        path: compiler.lower_Path,
    ): Promise<compiler.WithInfo<compiler.Info, AnyDeclaration> | null> {
        for (const declaration of this.declarations) {
            if (declaration.item.path === path) {
                return declaration;
            }
        }

        return null;
    }

    async getDeclarationFromInfo(
        info: compiler.Info,
        between: boolean,
    ): Promise<compiler.WithInfo<compiler.Info, AnyDeclaration> | null> {
        for (const declaration of this.declarations) {
            if (this.compareInfo(declaration.info, info, between)) {
                return declaration;
            }
        }

        return null;
    }

    async renderSourceLocation(
        value: compiler.WithInfo<compiler.Info, unknown>,
    ): Promise<RenderedSourceLocation | null> {
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

    async renderCode(value: compiler.WithInfo<compiler.Info, unknown>): Promise<string | null> {
        const renderedSourceLocation = await this.renderSourceLocation(value);
        if (!renderedSourceLocation) {
            return null;
        }

        return this.files[renderedSourceLocation.path].code.slice(
            renderedSourceLocation.start.index,
            renderedSourceLocation.end.index,
        );
    }

    async renderDeclaration(
        declaration: compiler.WithInfo<compiler.Info, AnyDeclaration>,
    ): Promise<string | null> {
        switch (declaration.item.type) {
            case "type": {
                const typeFunction = await this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    [],
                    { kind: "arrow" },
                );

                return `${declaration.item.name} : ${typeFunction}type`;
            }
            case "trait": {
                const typeFunction = await this.renderTypeFunction(
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
                if (
                    declaration.item.declaration.parameters.length > 0 ||
                    declaration.item.declaration.bounds.length > 0
                ) {
                    const typeFunction = await this.renderTypeFunction(
                        declaration.item.declaration.parameters,
                        declaration.item.declaration.bounds,
                        { kind: "arrow" },
                    );

                    const type = await this.renderType(
                        declaration.item.declaration.type,
                        true,
                        false,
                        false,
                    );

                    return `${declaration.item.name} :: ${typeFunction}${type}`;
                } else {
                    const type = await this.renderType(
                        declaration.item.declaration.type,
                        true,
                        false,
                        false,
                    );
                    return `${declaration.item.name} :: ${type}`;
                }
            }
            case "instance": {
                const typeFunction = await this.renderTypeFunction(
                    declaration.item.declaration.parameters,
                    declaration.item.declaration.bounds,
                    { kind: "arrow" },
                );

                const instance = await this.renderInstance(declaration.item.declaration.instance);

                return `${typeFunction}instance ${instance}`;
            }
            default:
                declaration.item satisfies never;
                return null;
        }
    }

    async renderPattern(
        pattern: compiler.exhaustiveness_Pattern,
        isTopLevel: boolean,
    ): Promise<string> {
        switch (pattern.type) {
            case "constructor": {
                const [constructor, values] = pattern.value;
                switch (constructor.type) {
                    case "variant": {
                        const declaration = await this.getDeclarationFromPath(constructor.value);
                        if (!declaration) {
                            return "<unknown>";
                        }

                        const name = declaration.item.name ?? "<unknown>";

                        const rendered =
                            values.length === 0
                                ? name
                                : `${name} ${(
                                      await Promise.all(
                                          values.map((pattern) =>
                                              this.renderPattern(pattern, false),
                                          ),
                                      )
                                  ).join(" ")}`;

                        return isTopLevel || values.length === 0 ? rendered : `(${rendered})`;
                    }
                    case "tuple": {
                        const rendered =
                            values.length === 0
                                ? "()"
                                : values.length === 1
                                ? `${await this.renderPattern(values[0], isTopLevel)} ;`
                                : `${(
                                      await Promise.all(
                                          values.map((value) => this.renderPattern(value, false)),
                                      )
                                  ).join(" ; ")}`;

                        return isTopLevel || values.length === 0 ? rendered : `(${rendered})`;
                    }
                    case "structure": {
                        return "{ ... }";
                    }
                    case "wrapper": {
                        const declaration = await this.getDeclarationFromPath(constructor.value);
                        if (!declaration) {
                            return "<unknown>";
                        }

                        const rendered = `${declaration.item.name} ${await this.renderPattern(
                            values[0],
                            false,
                        )}`;

                        return isTopLevel ? rendered : `(${rendered})`;
                    }
                    case "unbounded": {
                        return "_";
                    }
                    default:
                        constructor satisfies never;
                        return "<unknown>";
                }
            }
            case "binding": {
                return "_";
            }
            case "or": {
                return "<unknown>";
            }
            default:
                pattern satisfies never;
                return "<unknown>";
        }
    }

    async renderType(
        type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
        isTopLevel: boolean,
        describe: boolean,
        renderAsCode: boolean,
    ): Promise<string> {
        if (isTopLevel && describe && this.interface) {
            const message = await this.configuration.describeType(this, type);

            if (message) {
                return await this.renderTypeLevelText(message, false);
            }
        }

        const render = (
            type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>,
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
                case "declared":
                case "alias": {
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

                    return isTopLevel || isReturn || type.item.value.parameters.length === 0
                        ? rendered
                        : `(${rendered})`;
                }
                case "function": {
                    const inputs = type.item.value.inputs
                        .map((input) => render(input, false, false))
                        .join(" ");

                    const output = render(type.item.value.output, false, true);

                    const rendered = `${inputs} -> ${output}`;

                    return isTopLevel || isReturn ? rendered : `(${rendered})`;
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

                    return isTopLevel || isReturn || type.item.value.length === 0
                        ? rendered
                        : `(${rendered})`;
                }
                case "block": {
                    return `{${render(type.item.value, true, false)}}`;
                }
                case "intrinsic": {
                    return "intrinsic";
                }
                case "message": {
                    let message = "";
                    for (const segment of type.item.value.segments) {
                        message += segment.text + render(segment.type, true, true);
                    }

                    message += type.item.value.trailing;

                    return message;
                }
            }
        };

        const rendered = render(type, isTopLevel, true);
        return renderAsCode ? `\`${rendered}\`` : rendered;
    }

    async renderTypeFunction(
        parameters: compiler.lower_Path[],
        bounds: compiler.WithInfo<compiler.Info, compiler.typecheck_Instance>[],
        format:
            | { kind: "arrow" }
            | {
                  kind: "description";
                  type: compiler.WithInfo<compiler.Info, compiler.typecheck_Type>;
              },
    ): Promise<string> {
        switch (format.kind) {
            case "arrow": {
                if (parameters.length === 0) {
                    return "";
                }

                const renderedParameters = parameters
                    .map((parameter) => this.nameForPath(parameter))
                    .join(" ");

                let renderedBounds = (
                    await Promise.all(
                        bounds.map(async (bound) => `${await this.renderInstance(bound)}`),
                    )
                ).join(" ");

                if (renderedBounds) {
                    renderedBounds = ` where ${renderedBounds}`;
                }

                return `${renderedParameters}${renderedBounds} => `;
            }
            case "description": {
                const renderedType = await this.renderType(format.type, true, false, true);

                switch (parameters.length) {
                    case 0: {
                        return renderedType;
                    }
                    case 1: {
                        return `${renderedType} for any type \`${this.nameForPath(
                            parameters[0],
                        )}\``;
                    }
                    default: {
                        const last = parameters.pop()!;

                        return `${await this.renderType(
                            format.type,
                            true,
                            false,
                            true,
                        )} for any types ${parameters
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

    async renderInstance(
        instance: compiler.WithInfo<compiler.Info, compiler.typecheck_Instance>,
    ): Promise<string> {
        const trait = this.nameForPath(instance.item.trait);

        const parameters = (
            await Promise.all(
                instance.item.parameters.map((type) => this.renderType(type, false, false, false)),
            )
        ).join(" ");

        return parameters.length === 0 ? trait : `(${trait} ${parameters})`;
    }

    async renderDiagnostic(
        diagnostic: compiler.WithInfo<compiler.Info, compiler.Diagnostic>,
    ): Promise<RenderedDiagnostic | null> {
        const renderedSourceLocation = (await this.renderSourceLocation(diagnostic)) ?? {
            path: diagnostic.info.location.path,
            visiblePath: diagnostic.info.location.visiblePath,
            start: { line: 0, column: 0, index: 0 },
            end: { line: 0, column: 0, index: 0 },
        };

        let severity: "warning" | "error";
        let message: string;
        let fix: RenderedFix | null = null;
        switch (diagnostic.item.type) {
            case "tokenize": {
                switch (diagnostic.item.value.type) {
                    case "invalidToken": {
                        severity = "error";
                        message = "unrecognized symbol";
                        fix = {
                            message: "remove this symbol",
                            replacement: "",
                        };
                        break;
                    }
                    case "mismatch": {
                        severity = "error";

                        const { expected, found } = diagnostic.item.value.value;
                        if (expected && found) {
                            message = `expected ${await this.renderToken(
                                expected,
                                "a",
                            )} here, but found ${await this.renderToken(found, "a")}`;
                        } else if (expected && !found) {
                            message = `expected ${await this.renderToken(expected, "a")} here`;
                        } else if (!expected && found) {
                            message = `unexpected ${await this.renderToken(found, "a")} here`;
                            fix = {
                                message: `remove ${await this.renderToken(found, "this")}`,
                                replacement: "",
                            };
                        } else {
                            return null;
                        }

                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            case "parse": {
                const { expected, stack, direction } = diagnostic.item.value;
                const context =
                    stack.length > 1
                        ? ` while reading this ${await this.renderSyntaxKind(
                              stack[stack.length - 2].item,
                          )}`
                        : "";

                switch (direction) {
                    case "before": {
                        severity = "error";
                        message = `expected ${await this.renderSyntaxKind(
                            expected,
                        )} before this${context}`;
                        break;
                    }
                    case "after": {
                        severity = "error";
                        message = `expected ${await this.renderSyntaxKind(
                            expected,
                        )} after this${context}`;
                        break;
                    }
                    case null: {
                        severity = "error";
                        message = `expected ${await this.renderSyntaxKind(
                            expected,
                        )} here${context}`;
                        break;
                    }
                    default:
                        direction satisfies never;
                        return null;
                }

                break;
            }
            case "syntax": {
                switch (diagnostic.item.value.type) {
                    case "unexpectedBound": {
                        severity = "error";
                        message = "bounds aren't allowed on type and trait definitions";
                        break;
                    }
                    case "expectedConstantValue": {
                        severity = "error";
                        message = `missing a value for \`${diagnostic.item.value.value}\` on the next line`;
                        break;
                    }
                    case "emptyTypeRepresentation": {
                        severity = "error";
                        message = "missing a field or variant between the braces in this type";
                        break;
                    }
                    case "expectedField": {
                        severity = "error";
                        message = "expected a field of the form `name :: Type` here";
                        break;
                    }
                    case "expectedVariant": {
                        severity = "error";
                        message = "expected a variant of the form `Name` here";
                        break;
                    }
                    case "invalidTextLiteral": {
                        severity = "error";
                        message = diagnostic.item.value.value.error;
                        break;
                    }
                    case "invalidPlaceholderText": {
                        const { expected, found } = diagnostic.item.value.value;
                        severity = "error";
                        message = `text has ${expected} placeholders, but ${found} inputs were provided here`;
                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            case "lower": {
                switch (diagnostic.item.value.type) {
                    case "unresolvedName": {
                        severity = "error";
                        message = `can't find \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedType": {
                        severity = "error";
                        message = `can't find type \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedTrait": {
                        severity = "error";
                        message = `can't find trait \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedVariant": {
                        severity = "error";
                        message = `can't find variant \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "unresolvedLanguageItem": {
                        severity = "error";
                        message = `can't find language item \`${diagnostic.item.value.value}\``;
                        break;
                    }
                    case "ambiguousName": {
                        severity = "error";
                        message = `\`${diagnostic.item.value.value.name}\` has multiple definitions`;
                        break;
                    }
                    case "alreadyDefined": {
                        severity = "error";
                        message = `\`${diagnostic.item.value.value}\` is already defined`;
                        break;
                    }
                    case "nestedLanguageDeclaration": {
                        severity = "error";
                        message = "language items must be declared at the top level";
                        break;
                    }
                    case "notAWrapper": {
                        severity = "error";
                        message =
                            "this pattern matches a structure or enumeration, not a wrapper type";
                        break;
                    }
                    case "wrapperExpectsASinglePattern": {
                        severity = "error";
                        message = "expected a single pattern after the name of the type";
                        break;
                    }
                    case "invalidMutatePattern": {
                        severity = "error";
                        message = "`!` only works when assigning to a variable using `:`";
                        break;
                    }
                    case "missingTypes": {
                        severity = "error";
                        message = `missing ${diagnostic.item.value.value} types here`;
                        break;
                    }
                    case "extraType": {
                        severity = "error";
                        message = "extra type provided here";
                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            case "typecheck": {
                switch (diagnostic.item.value.type) {
                    case "recursionLimit": {
                        severity = "error";
                        message = "this code is too complex to check; try simplifying it";
                        break;
                    }
                    case "missingLanguageItem": {
                        severity = "error";
                        message = `checking this code requires the \`${diagnostic.item.value.value}\` language item`;
                        break;
                    }
                    case "unknownType": {
                        severity = "error";

                        const renderedType = await this.renderType(
                            { info: diagnostic.info, item: diagnostic.item.value.value },
                            true,
                            false,
                            true,
                        );

                        if (renderedType === "`_`") {
                            message = "could not determine what kind of value this code produces";
                        } else {
                            message = `this code produces ${renderedType}, but the \`_\`s are unknown`;
                        }

                        break;
                    }
                    case "undeclaredTypeParameter": {
                        severity = "error";
                        message = `this code references the type parameter \`${diagnostic.item.value.value}\`, which isn't available here`;
                        break;
                    }
                    case "mismatch": {
                        const { expected, actual } = diagnostic.item.value.value;

                        severity = "error";

                        let expectedMessage = await this.renderType(expected, true, true, true);
                        let actualMessage = await this.renderType(actual, true, true, true);

                        // If the type descriptions are equal, try rendering the
                        // actual type by setting `describe` to false
                        if (expectedMessage === actualMessage) {
                            expectedMessage = await this.renderType(expected, true, false, true);
                            actualMessage = await this.renderType(actual, true, false, true);
                        }

                        message = `expected ${expectedMessage} here, but found ${actualMessage}`;

                        break;
                    }
                    case "missingInputs": {
                        const code = await this.renderCode(diagnostic);

                        const inputs = await Promise.all(
                            diagnostic.item.value.value.map((type) =>
                                this.renderType(type, true, true, true),
                            ),
                        );

                        severity = "error";

                        switch (inputs.length) {
                            case 1:
                                message = `missing ${inputs[0]} for \`${code}\``;
                                break;
                            case 2:
                                message = `missing ${inputs[0]} and ${inputs[1]} for \`${code}\``;
                                break;
                            default:
                                message = `missing ${inputs.slice(0, -1).join(", ")}, and ${
                                    inputs[inputs.length - 1]
                                } for \`${code}\``;

                                break;
                        }

                        break;
                    }
                    case "extraInput": {
                        const code = await this.renderCode(diagnostic);
                        severity = "error";
                        message = `extra input provided to \`${code}\``;
                        break;
                    }
                    case "unresolvedInstance": {
                        const code = await this.renderCode(diagnostic);
                        const renderedInstance = await this.renderInstance({
                            info: diagnostic.info,
                            item: diagnostic.item.value.value.instance,
                        });
                        severity = "error";
                        message = `\`${code}\` requires \`${renderedInstance}\``;
                        break;
                    }
                    case "traitHasNoValue": {
                        const code = await this.renderCode(diagnostic);
                        severity = "error";
                        message = `\`${code}\` can't be used as a value`;
                        break;
                    }
                    case "expectedInstanceValue": {
                        severity = "error";
                        message = "`instance` declaration is missing a value";
                        break;
                    }
                    case "unexpectedInstanceValue": {
                        severity = "error";
                        message = "`instance` declaration declares a value, but the trait doesn't";
                        break;
                    }
                    case "notAStructure": {
                        const renderedType = await this.renderType(
                            diagnostic.item.value.value,
                            true,
                            true,
                            true,
                        );
                        severity = "error";
                        message = `${renderedType} is not a structure`;
                        break;
                    }
                    case "missingFields": {
                        const renderedFields = diagnostic.item.value.value
                            .map((field) => `\`${field}\``)
                            .join(", ");
                        severity = "error";
                        message = `missing fields ${renderedFields}`;
                        break;
                    }
                    case "extraField": {
                        severity = "error";
                        message = "extra field";
                        break;
                    }
                    case "overlappingInstances": {
                        severity = "error";
                        message =
                            "this instance already exists elsewhere; try making it more specific";
                        break;
                    }
                    case "missingPatterns": {
                        const patterns = diagnostic.item.value.value;
                        const last = patterns.pop()!;

                        severity = "error";
                        message =
                            patterns.length === 0
                                ? last.type === "binding"
                                    ? "missing variable to handle remaining patterns"
                                    : `this code doesn't handle ${await this.renderPattern(
                                          last,
                                          true,
                                      )}`
                                : `this code doesn't handle ${(
                                      await Promise.all(
                                          patterns.map((pattern) =>
                                              this.renderPattern(pattern, true),
                                          ),
                                      )
                                  ).join(", ")} or ${await this.renderPattern(last, true)}`;
                        break;
                    }
                    case "extraPattern": {
                        severity = "warning";
                        message = "this pattern is unnecessary because it is already handled above";
                        fix = {
                            message: "remove this pattern",
                            replacement: "",
                        };
                        break;
                    }
                    case "custom": {
                        severity = "error";

                        message = await this.renderTypeLevelText(
                            diagnostic.item.value.value.message,
                            true,
                        );

                        if (diagnostic.item.value.value.fix) {
                            fix = {
                                message: await this.renderTypeLevelText(
                                    diagnostic.item.value.value.fix[0],
                                    true,
                                ),
                                replacement: await this.renderTypeLevelText(
                                    diagnostic.item.value.value.fix[1],
                                    false,
                                ),
                            };
                        }

                        break;
                    }
                    default:
                        diagnostic.item.value satisfies never;
                        return null;
                }

                break;
            }
            case "ir": {
                severity = "error";
                message = "failed to produce IR for this code";
                break;
            }
            default:
                diagnostic.item satisfies never;
                return null;
        }

        return {
            location: renderedSourceLocation,
            severity,
            message,
            fix,
        };
    }

    async renderTypeLevelText(
        text: compiler.typecheck_MessageType,
        renderAsCode: boolean,
    ): Promise<string> {
        const renderSegmentsAsCode =
            (text.segments.length === 0
                ? text.trailing.startsWith("`")
                : text.segments[0].text.startsWith("`")) && text.trailing.endsWith("`");

        let message = "";
        for (const segment of text.segments) {
            const code =
                renderSegmentsAsCode || segment.text.slice(segment.text.length - 1) === "`"
                    ? await this.renderCode(segment.type)
                    : null;
            message +=
                segment.text + (code ?? (await this.renderType(segment.type, true, true, true)));
        }
        message += text.trailing;

        if (renderSegmentsAsCode && !renderAsCode) {
            message = message.slice(1, -1);
        }

        return message;
    }

    async renderTypeRole(role: compiler.typecheck_Role): Promise<string> {
        switch (role) {
            case "pattern":
                return "pattern";
            case "annotation":
                return "annotation";
            case "trait":
                return "trait";
            case "instance":
                return "instance";
            case "structureField":
                return "structure field";
            case "variantElement":
                return "variant element";
            case "wrappedType":
                return "wrapped type";
            case "functionInput":
                return "function input";
            case "functionOutput":
                return "function output";
            case "bound":
                return "bound";
            case "defaultType":
                return "default type";
            case "variable":
                return "variable";
            case "typeParameter":
                return "type parameter";
            case "emptyBlock":
                return "empty block";
            case "whenArm":
                return "when arm";
            case "collectionElement":
                return "collection element";
        }
    }

    async renderToken(token: compiler.Token, prefix: "a" | "this"): Promise<string> {
        const altPrefix = prefix === "a" ? "the" : "this";

        switch (token.type) {
            case "number":
                return `${prefix} number`;
            case "leftParenthesis":
                return `${prefix} opening parenthesis (\`(\`)`;
            case "rightParenthesis":
                return `${prefix} closing parenthesis (\`)\`)`;
            case "leftBracket":
                return `${prefix} opening bracket (\`[\`)`;
            case "rightBracket":
                return `${prefix} closing bracket (\`]\`)`;
            case "leftBrace":
                return `${prefix} opening brace (\`{\`)`;
            case "rightBrace":
                return `${prefix} closing brace (\`}\`)`;
            case "lineBreak":
                return `${altPrefix} end of the line`;
            case "comment":
                return `${prefix} comment`;
            case "keyword":
                return `${altPrefix} word \`${token.value}\``;
            case "operator":
                return `${altPrefix} symbol \`${token.value}\``;
            case "variadicOperator":
                return `${altPrefix} symbol \`${token.value}\``;
            case "nonAssociativeOperator":
                return `${altPrefix} symbol \`${token.value}\``;
            case "name":
                return `${prefix} name`;
            case "text":
                return `${prefix} piece of text`;
        }
    }

    async renderSyntaxKind(kind: compiler.syntax_SyntaxKind): Promise<string> {
        switch (kind) {
            case "number":
                return "number";
            case "name":
                return "name";
            case "text":
                return "text";
            case "topLevel":
                return "top level";
            case "attribute":
                return "attribute";
            case "attributeValue":
                return "attribute value";
            case "statement":
                return "statement";
            case "keyword":
                return "keyword";
            case "operator":
                return "operator";
            case "instance":
                return "instance";
            case "typeParameter":
                return "type parameter";
            case "pattern":
                return "pattern";
            case "wildcardPattern":
                return "wildcard pattern";
            case "numberPattern":
                return "number pattern";
            case "textPattern":
                return "text pattern";
            case "variantPattern":
                return "variant pattern";
            case "destructurePattern":
                return "destructure pattern";
            case "tuplePattern":
                return "tuple pattern";
            case "orPattern":
                return "or pattern";
            case "mutatePattern":
                return "mutate pattern";
            case "annotatePattern":
                return "annotate pattern";
            case "expression":
                return "expression";
            case "type":
                return "type";
            case "placeholderType":
                return "placeholder type";
            case "declaredType":
                return "declared type";
            case "functionType":
                return "function type";
            case "tupleType":
                return "tuple type";
            case "blockType":
                return "block type";
            case "intrinsicType":
                return "intrinsic type";
            case "messageType":
                return "message type";
            case "typeMember":
                return "type member";
            case "fieldDeclaration":
                return "field declaration";
            case "variantDeclaration":
                return "variant declaration";
            case "arm":
                return "arm";
            case "typeFunction":
                return "type function";
            case "typeRepresentation":
                return "type representation";
            case "typeDeclaration":
                return "type declaration";
            case "typeAliasDeclaration":
                return "type alias declaration";
            case "traitDeclaration":
                return "trait declaration";
            case "instanceDeclaration":
                return "instance declaration";
            case "constantDeclaration":
                return "constant declaration";
            case "languageDeclaration":
                return "language declaration";
            case "assignment":
                return "assignment";
            case "annotateExpression":
                return "annotate expression";
            case "nameExpression":
                return "name expression";
            case "numberExpression":
                return "number expression";
            case "textExpression":
                return "text expression";
            case "doExpression":
                return "do expression";
            case "callExpression":
                return "call expression";
            case "applyExpression":
                return "apply expression";
            case "binaryOperatorExpression":
                return "binary operator expression";
            case "asExpression":
                return "as expression";
            case "isExpression":
                return "is expression";
            case "whenExpression":
                return "when expression";
            case "intrinsicExpression":
                return "intrinsic expression";
            case "tupleExpression":
                return "tuple expression";
            case "collectionExpression":
                return "collection expression";
            case "structureExpression":
                return "structure expression";
            case "structureField":
                return "structure field";
            case "whenBody":
                return "when body";
            case "whenArm":
                return "when arm";
            case "blockExpression":
                return "block expression";
            case "functionExpression":
                return "function expression";
            case "functionInputs":
                return "function inputs";
            case "nothing":
                return "nothing";
        }
    }

    async renderDiagnosticToDebugString(diagnostic: RenderedDiagnostic): Promise<string> {
        const line = diagnostic.location.start.line + 1;
        const column = diagnostic.location.start.column + 1;

        return `${diagnostic.location.visiblePath}:${line}:${column}: ${diagnostic.severity}: ${diagnostic.message}`;
    }

    async renderDocumentation(
        declaration: compiler.WithInfo<compiler.Info, AnyDeclaration>,
    ): Promise<RenderedDocumentation | null> {
        const renderedSourceLocation = await this.renderSourceLocation(declaration);
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

        const docs = docLines.join("\n");

        let example: string | null = null;
        if ("attributes" in declaration.item.declaration) {
            for (const attribute of declaration.item.declaration.attributes) {
                if (
                    attribute.item.type === "valued" &&
                    attribute.item.value.value.item.type === "text" &&
                    attribute.item.value.name.item === "example"
                ) {
                    example = attribute.item.value.value.item.value.item;
                    break;
                }
            }
        }

        return { docs, example };
    }

    async renderHighlight(
        value: compiler.WithInfo<compiler.Info, unknown>,
    ): Promise<RenderedHighlight | null> {
        if (!this.interface) {
            return null;
        }

        const declaration = await this.getDeclarationFromInfo(value.info, false);

        if (!declaration) {
            return null;
        }

        const options: RenderedHighlight = {};
        if ("attributes" in declaration.item.declaration) {
            for (const attribute of declaration.item.declaration.attributes) {
                if (
                    attribute.item.type === "valued" &&
                    attribute.item.value.value.item.type === "text"
                ) {
                    switch (attribute.item.value.name.item) {
                        case "highlight-category": {
                            options.category = attribute.item.value.value.item.value.item;
                            break;
                        }
                        case "highlight-icon": {
                            options.icon = attribute.item.value.value.item.value.item;
                            break;
                        }
                        default: {
                            break;
                        }
                    }
                }
            }
        }

        if (!options.category && !options.icon) {
            return null;
        }

        return options;
    }

    async renderSuggestionsAtCursor(path: string, index: number): Promise<RenderedSuggestion[]> {
        const keywordSuggestions = keywords.map(
            (keyword): RenderedSuggestion => ({
                kind: "keyword",
                name: keyword,
                code: null,
                docs: null, // TODO: Documentation for keywords
            }),
        );

        const operatorSuggestions = operators.map(
            (operator): RenderedSuggestion => ({
                kind: "operator",
                name: operator,
                code: null,
                docs: null, // TODO: Documentation for keywords
            }),
        );

        const getDeclarations = (kind: Exclude<AnyDeclaration["type"], "instance">) =>
            Object.entries(this.interface?.[`${kind}Declarations`] ?? {}).map(
                ([path, declaration]): [
                    string,
                    compiler.WithInfo<compiler.Info, AnyDeclaration>,
                ] => [
                    path,
                    {
                        info: declaration.info,
                        item: {
                            type: kind,
                            path,
                            name: this.nameForPath(path),
                            declaration: declaration.item,
                        },
                    },
                ],
            );

        const getSuggestions = (kind: Exclude<AnyDeclaration["type"], "instance">) =>
            Promise.all(
                getDeclarations(kind).map(
                    async ([path, declaration]): Promise<RenderedSuggestion> => ({
                        kind,
                        name: this.nameForPath(path),
                        code: await this.renderDeclaration(declaration),
                        docs: await this.renderDocumentation(declaration),
                    }),
                ),
            );

        const typeSuggestions = await getSuggestions("type");
        const traitSuggestions = await getSuggestions("trait");
        const constantSuggestions = await getSuggestions("constant");
        const localSuggestions = await Promise.all(
            (
                await this.getLocalsAtCursor(path, index)
            ).map(
                async (declaration): Promise<RenderedSuggestion> => ({
                    kind: declaration.item.type as "variable" | "typeParameter",
                    name: declaration.item.name!,
                    code: await this.renderDeclaration(declaration),
                    docs: null, // locals cannot have documentation
                }),
            ),
        );

        return [
            ...keywordSuggestions,
            ...operatorSuggestions,
            ...typeSuggestions,
            ...traitSuggestions,
            ...constantSuggestions,
            ...localSuggestions,
        ];
    }

    private async getLocalsAtCursor(path: string, index: number) {
        const expressionTree = this.getExpressionTreeAtCursor(path, index);
        if (!expressionTree) {
            return [];
        }

        const locals: compiler.WithInfo<compiler.Info, AnyDeclaration>[] = [];

        const info = expressionTree[0].info;
        if (info) {
            const declaration = await this.getDeclarationFromInfo(info, true);
            if (declaration?.item.type === "constant" || declaration?.item.type === "instance") {
                for (const typeParameter of declaration.item.declaration.parameters) {
                    const typeParameterDeclaration = await this.getDeclarationFromPath(
                        typeParameter,
                    );
                    if (typeParameterDeclaration?.item.name) {
                        locals.push(typeParameterDeclaration);
                    }
                }
            }
        }

        for (const expression of expressionTree) {
            if (expression.item.kind.type === "variable") {
                const [_name, path] = expression.item.kind.value;

                const declaration = await this.getDeclarationFromPath(path);
                if (!declaration) {
                    continue;
                }

                locals.push(declaration);
            }
        }

        return locals;
    }

    async getPathAtCursor(
        path: string,
        index: number,
    ): Promise<compiler.WithInfo<compiler.Info, compiler.lower_Path> | null> {
        if (!this.ide) {
            return null;
        }

        for (const symbol of this.ide.symbols) {
            if (this.compareCursorWithInfo(path, index, symbol.info)) {
                return symbol;
            }
        }

        return null;
    }

    async getExpressionAtCursor(
        path: string,
        index: number,
    ): Promise<compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression> | null> {
        const expressionTree = this.getExpressionTreeAtCursor(path, index);
        if (!expressionTree) {
            return null;
        }

        return expressionTree[0];
    }

    private getExpressionTreeAtCursor(
        path: string,
        index: number,
    ): compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression>[] | null {
        for (const item of this.libraries.flatMap((library) => Object.values(library.items))) {
            if (!this.compareCursorWithInfo(path, index, item.expression.info)) {
                continue;
            }

            const candidates: compiler.WithInfo<
                compiler.Info,
                compiler.typecheck_TypedExpression
            >[] = [];

            this.traverseExpression(item.expression, (expression) => {
                if (
                    index >= expression.info.location.span.start &&
                    index < expression.info.location.span.end
                ) {
                    candidates.push(expression);
                }

                return false;
            });

            if (candidates.length === 0) {
                continue;
            }

            candidates.sort((left, right) => {
                const length = (
                    expression: compiler.WithInfo<
                        compiler.Info,
                        compiler.typecheck_TypedExpression
                    >,
                ) => expression.info.location.span.end - expression.info.location.span.start;

                return length(left) - length(right);
            });

            return candidates;
        }

        return null;
    }

    private traverseExpression(
        expression: compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression>,
        f: (
            expression: compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression>,
        ) => boolean,
    ): compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression> | null {
        const traverse = (
            expression: compiler.WithInfo<compiler.Info, compiler.typecheck_TypedExpression>,
        ) => {
            if (f(expression)) {
                throw expression;
            }

            switch (expression.item.kind.type) {
                case "function":
                    traverse(expression.item.kind.value.body);
                    break;
                case "block":
                    for (const statement of expression.item.kind.value.statements) {
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

    private compareInfo(left: compiler.Info, right: compiler.Info, between: boolean): boolean {
        return (
            left.location.visiblePath === right.location.visiblePath &&
            (between
                ? left.location.span.start >= right.location.span.start
                : left.location.span.start === right.location.span.start) &&
            (between
                ? left.location.span.end <= right.location.span.end
                : left.location.span.end === right.location.span.end)
        );
    }

    private compareCursorWithInfo(path: string, index: number, info: compiler.Info): boolean {
        // HACK: The top level has a span of 0..0
        const isTopLevel = info.location.span.start === 0 && info.location.span.end === 0;

        return (
            isTopLevel ||
            (path === info.location.visiblePath &&
                index >= info.location.span.start &&
                index < info.location.span.end)
        );
    }

    private nameForPath(path: compiler.lower_Path): string {
        return path.split(" / ").pop()?.split(" ").pop() ?? "<unknown>";
    }
}
