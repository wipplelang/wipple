import * as vscode from "vscode";
import * as execa from "execa";
import * as _ from "lodash";
import * as path from "path";
import { Diagnostic, Expression, formatType, Program, Span, traverseExpr } from "./models";

const debug = process.env.WIPPLE_VSCODE_DEVELOPMENT === "1";

const storage: {
    [uri: string]: {
        program: Program;
        diagnostics: Diagnostic[];
    };
} = {};

const semanticTokensEvent = new vscode.EventEmitter<void>();
let diagnosticCollection: vscode.DiagnosticCollection;

export const activate = (context: vscode.ExtensionContext) => {
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument(async (document) => {
            await handleDocumentUpdate(document);
            semanticTokensEvent.fire();
        })
    );

    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument((event) => handleDocumentUpdate(event.document))
    );

    diagnosticCollection = vscode.languages.createDiagnosticCollection("wipple");
    context.subscriptions.push(diagnosticCollection);

    context.subscriptions.push(
        vscode.languages.registerDocumentSemanticTokensProvider(
            "wipple",
            semanticTokensProvider,
            semanticTokensLegend
        )
    );
    context.subscriptions.push(semanticTokensEvent);

    context.subscriptions.push(vscode.languages.registerHoverProvider("wipple", hoverProvider));
};

const handleDocumentUpdate = async (document: vscode.TextDocument) => {
    const source = document.getText();

    const bin = debug ? path.join(__dirname, "../../../target/release/wipple") : "wipple";
    const args = debug ? ["--std", path.join(__dirname, "../../../pkg/std/std.wpl")] : [];

    let output: execa.ExecaReturnBase<string>;
    try {
        output = await execa.execa(
            bin,
            [
                "compile",
                "-",
                "--base-path",
                path.dirname(document.uri.fsPath),
                "--target",
                "analysis",
                ...args,
            ],
            { input: source }
        );
    } catch (e) {
        output = e as execa.ExecaError;
    }

    const result = JSON.parse(output.stdout);
    storage[document.uri.toString()] = result;

    await updateDiagnostics(document, result.diagnostics);
};

const belongsToCurrentDocument = (span: Span) =>
    span.path.type === "Virtual" && span.path.value === "stdin";

const updateDiagnostics = async (
    document: vscode.TextDocument,
    programDiagnostics: Diagnostic[]
) => {
    const diagnostics: vscode.Diagnostic[] = [];

    for (const diagnostic of programDiagnostics) {
        const primaryNote = diagnostic.notes[0];
        if (!belongsToCurrentDocument(primaryNote.span)) {
            continue;
        }

        let severity: vscode.DiagnosticSeverity;
        switch (diagnostic.level) {
            case "Error":
                severity = vscode.DiagnosticSeverity.Error;
                break;
            case "Warning":
                severity = vscode.DiagnosticSeverity.Warning;
                break;
            case "Note":
                severity = vscode.DiagnosticSeverity.Information;
                break;
        }

        const start = document.positionAt(primaryNote.span.start);
        const end = document.positionAt(primaryNote.span.end);

        diagnostics.push({
            severity,
            message: `${diagnostic.message}\n${primaryNote.message}`,
            relatedInformation: diagnostic.notes.slice(1).map((note) => {
                const start = document.positionAt(note.span.start);
                const end = document.positionAt(note.span.end);

                return {
                    message: note.message,
                    location: {
                        uri: document.uri,
                        range: new vscode.Range(start, end),
                    },
                };
            }),
            range: new vscode.Range(start, end),
            source: "wipple",
        });
    }

    diagnosticCollection.set(document.uri, diagnostics);
};

const semanticTokensLegend = new vscode.SemanticTokensLegend(
    [
        "function",
        "keyword",
        "operator",
        "template",
        "type",
        "typeParameter",
        "interface",
        "constant",
        "variable",
    ],
    ["declaration"]
);

const semanticTokensProvider: vscode.DocumentSemanticTokensProvider = {
    provideDocumentSemanticTokens: (document) => {
        const builder = new vscode.SemanticTokensBuilder(semanticTokensLegend);

        const s = storage[document.uri.toString()];
        if (!s) return builder.build();
        const { program } = s;

        let tokens: [Span, string, string[]][] = [];
        const add = (span: Span, type: string, modifiers: string[]) => {
            if (!belongsToCurrentDocument(span)) return;

            tokens.push([span, type, modifiers]);
        };

        const addExpr = (expr: Expression) => {
            if (!["Variable", "Constant"].includes(expr.kind.type)) return;

            switch (expr.ty.type) {
                case "Function":
                    add(expr.span, "function", []);
                    break;
                default:
                    break;
            }
        };

        for (const expr of program.body) {
            traverseExpr(expr, addExpr);
        }

        for (const constant of Object.values(program.declarations.generic_constants)) {
            traverseExpr(constant.decl.value, addExpr);
        }

        for (const operator of Object.values(program.declarations.operators)) {
            const template = program.declarations.templates[operator.template];

            add(template.span, "operator", ["declaration"]);

            for (const use of template.uses) {
                add(use, "operator", []);
            }
        }

        for (const template of Object.values(program.declarations.templates)) {
            const type = template.attributes.keyword ? "keyword" : "template";

            add(template.span, type, ["declaration"]);

            for (const use of template.uses) {
                add(use, type, []);
            }
        }

        for (const type of Object.values(program.declarations.types)) {
            add(type.span, "type", ["declaration"]);

            for (const use of type.uses) {
                add(use, "type", []);
            }
        }

        for (const param of Object.values(program.declarations.type_parameters)) {
            add(param.span, "typeParameter", ["declaration"]);

            for (const use of param.uses) {
                add(use, "typeParameter", []);
            }
        }

        for (const trait of Object.values(program.declarations.traits)) {
            add(trait.span, "interface", ["declaration"]);

            for (const use of trait.uses) {
                add(use, "interface", []);
            }
        }

        for (const [_file, _genericId, constant] of Object.values(
            program.declarations.monomorphized_constants
        )) {
            add(constant.span, "constant", ["declaration"]);

            for (const use of constant.uses) {
                add(use, "constant", []);
            }
        }

        for (const variable of Object.values(program.declarations.variables)) {
            add(variable.span, "variable", ["declaration"]);

            for (const use of variable.uses) {
                add(use, "variable", []);
            }
        }

        tokens.sort(([left], [right]) => left.start - right.start);

        tokens = _.uniqBy(tokens, ([span]) => span.start);

        for (const [span, type, modifiers] of tokens) {
            const start = document.positionAt(span.start);
            const end = document.positionAt(span.end);

            if (end.line === start.line) {
                builder.push(new vscode.Range(start, end), type, modifiers);
            }
        }

        return builder.build();
    },
    onDidChangeSemanticTokens: semanticTokensEvent.event,
};

const hoverProvider: vscode.HoverProvider = {
    provideHover: async (document, position) => {
        const s = storage[document.uri.toString()];
        if (!s) return undefined;
        const { program } = s;

        const offset = document.offsetAt(position);

        let info: [Span, vscode.Hover][] = [];

        const isWithinHover = (span: Span) =>
            belongsToCurrentDocument(span) && offset >= span.start && offset <= span.end;

        const codeSegment = (code: string) => "```wipple\n" + code + "\n```";

        const addExpr = (expr: Expression) => {
            if (["Variable", "Constant"].includes(expr.kind.type)) {
                return;
            }

            if (!isWithinHover(expr.span)) return;

            const range = new vscode.Range(
                document.positionAt(expr.span.start),
                document.positionAt(expr.span.end)
            );

            const contents = [codeSegment(formatType(expr.ty, program))];
            info.push([expr.span, new vscode.Hover(contents, range)]);
        };

        for (const expr of program.body) {
            traverseExpr(expr, addExpr);
        }

        for (const constant of Object.values(program.declarations.generic_constants)) {
            traverseExpr(constant.decl.value, addExpr);
        }

        for (const template of Object.values(program.declarations.templates)) {
            for (const span of [template.span, ...template.uses]) {
                if (!isWithinHover(span)) continue;

                const range = new vscode.Range(
                    document.positionAt(span.start),
                    document.positionAt(span.end)
                );

                const contents: string[] = [];

                const help = template.attributes.help;
                if (help) contents.push(help.join("\n"));

                info.push([span, new vscode.Hover(contents, range)]);
            }
        }

        for (const type of Object.values(program.declarations.types)) {
            for (const span of [type.span, ...type.uses]) {
                if (!isWithinHover(span)) continue;

                const range = new vscode.Range(
                    document.positionAt(span.start),
                    document.positionAt(span.end)
                );

                const contents: string[] = [];

                contents.push(codeSegment(`${type.name!} : type`));

                const help = type.value.attributes.help;
                if (help) contents.push(help.join("\n"));

                info.push([span, new vscode.Hover(contents, range)]);
            }
        }

        for (const param of Object.values(program.declarations.type_parameters)) {
            for (const span of [param.span, ...param.uses]) {
                if (!isWithinHover(span)) continue;

                const range = new vscode.Range(
                    document.positionAt(span.start),
                    document.positionAt(span.end)
                );

                const contents: string[] = [];

                contents.push(codeSegment(`${param.name!} : type`));

                info.push([span, new vscode.Hover(contents, range)]);
            }
        }

        for (const trait of Object.values(program.declarations.traits)) {
            for (const span of [trait.span, ...trait.uses]) {
                if (!isWithinHover(span)) continue;

                const range = new vscode.Range(
                    document.positionAt(span.start),
                    document.positionAt(span.end)
                );

                const contents: string[] = [];

                contents.push(codeSegment(`${trait.name!} : type`));

                const help = trait.value.attributes.decl_attributes.help;
                if (help) contents.push(help.join("\n"));

                info.push([span, new vscode.Hover(contents, range)]);
            }
        }

        for (const [_file, genericId, monomorphizedConstant] of Object.values(
            program.declarations.monomorphized_constants
        )) {
            const genericConstant = program.declarations.generic_constants[genericId];

            for (const span of [monomorphizedConstant.span, ...monomorphizedConstant.uses]) {
                if (!isWithinHover(span)) continue;

                const range = new vscode.Range(
                    document.positionAt(span.start),
                    document.positionAt(span.end)
                );

                const contents: string[] = [];

                contents.push(
                    codeSegment(
                        `${genericConstant.decl.name!} :: ${formatType(
                            genericConstant.decl.value.ty,
                            program
                        )}`
                    )
                );

                const help = genericConstant.attributes?.help;
                if (help) contents.push(help.join("\n"));

                info.push([span, new vscode.Hover(contents, range)]);
            }
        }

        for (const variable of Object.values(program.declarations.variables)) {
            for (const span of [variable.span, ...variable.uses]) {
                if (!isWithinHover(span)) continue;

                const range = new vscode.Range(
                    document.positionAt(span.start),
                    document.positionAt(span.end)
                );

                const contents: string[] = [];

                contents.push(
                    codeSegment(`${variable.name!} :: ${formatType(variable.value, program)}`)
                );

                info.push([span, new vscode.Hover(contents, range)]);
            }
        }

        if (info.length === 0) return undefined;

        const [_span, hover] = _.sortBy(info, ([span]) => span.end - span.start)[0];
        return hover;
    },
};
