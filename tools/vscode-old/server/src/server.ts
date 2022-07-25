import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    DidChangeConfigurationNotification,
    TextDocumentSyncKind,
    InitializeResult,
    Diagnostic as LspDiagnostic,
    DiagnosticSeverity,
    SemanticTokenTypes,
    SemanticTokensRequest,
    DocumentUri,
    SemanticTokensBuilder,
    SemanticTokensRefreshRequest,
    SemanticTokensDeltaRequest,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";
import execa from "execa";
import { Program, Diagnostic, Span, Expression } from "./models";
import _ from "lodash";

const debug = process.env.WIPPLE_LSP_DEVELOPMENT === "1";

const tokenTypes = [
    SemanticTokenTypes.keyword,
    SemanticTokenTypes.operator,
    SemanticTokenTypes.macro,
    SemanticTokenTypes.type,
    SemanticTokenTypes.typeParameter,
    SemanticTokenTypes.interface,
    SemanticTokenTypes.variable,
    SemanticTokenTypes.function,
];

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
connection.onInitialize((params: InitializeParams) => {
    const capabilities = params.capabilities;

    hasConfigurationCapability = !!(
        capabilities.workspace && !!capabilities.workspace.configuration
    );
    hasWorkspaceFolderCapability = !!(
        capabilities.workspace && !!capabilities.workspace.workspaceFolders
    );

    const result: InitializeResult = {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            semanticTokensProvider: {
                documentSelector: [{ language: "wipple" }],
                full: true,
                legend: {
                    tokenTypes,
                    tokenModifiers: [],
                },
            },
            // completionProvider: {
            //     resolveProvider: true,
            // },
        },
    };

    if (hasWorkspaceFolderCapability) {
        result.capabilities.workspace = {
            workspaceFolders: {
                supported: true,
            },
        };
    }
    return result;
});

connection.onInitialized(() => {
    if (hasConfigurationCapability) {
        connection.client.register(DidChangeConfigurationNotification.type, undefined);
    }
});

const storage: Record<DocumentUri, [string, TextDocument, Program]> = {};

documents.onDidChangeContent(async (change) => {
    const source = change.document.getText();

    if (!source) {
        delete storage[change.document.uri];
        connection.sendDiagnostics({ uri: change.document.uri, diagnostics: [] });
        return;
    }

    const uri = new URL(change.document.uri);
    const path = uri.protocol === "file:" ? uri.pathname : "-";

    const bin = debug ? `${process.cwd()}/target/release/wipple` : "wipple";
    const args = debug ? ["--std", `${process.cwd()}/pkg/std/std.wpl`] : [];

    let output: execa.ExecaReturnBase<string>;
    try {
        output = await execa(bin, ["compile", path, "--target", "analysis", ...args], {
            input: path === "-" ? source : undefined,
        });
    } catch (e) {
        output = e as execa.ExecaError;
    }

    const result: { program: Program; diagnostics: Diagnostic[] } = JSON.parse(output.stdout);

    storage[change.document.uri] = [path, change.document, result.program];

    const diagnostics = convertDiagnostics(path, change.document, result.diagnostics);
    connection.sendDiagnostics({ uri: change.document.uri, diagnostics });

    connection.sendRequest(SemanticTokensRefreshRequest.method);
});

const convertDiagnostics = (path: string, document: TextDocument, diagnostics: Diagnostic[]) =>
    diagnostics.flatMap((diagnostic) => {
        const primaryNote = diagnostic.notes[0];
        if (primaryNote.span.path.value !== "stdin") {
            return [];
        }

        let severity: DiagnosticSeverity;
        switch (diagnostic.level) {
            case "Error":
                severity = 1;
                break;
            case "Warning":
                severity = 2;
                break;
            case "Note":
                severity = 3;
                break;
        }

        const start = document.positionAt(primaryNote.span.start);
        const end = document.positionAt(primaryNote.span.end);

        const lspDiagnostic: LspDiagnostic = {
            severity,
            message: `${diagnostic.message}\n${primaryNote.message}`,
            relatedInformation: diagnostic.notes.slice(1).map((note) => {
                const start = document.positionAt(note.span.start);
                const end = document.positionAt(note.span.end);

                return {
                    message: note.message,
                    location: {
                        uri:
                            note.span.path.value === path
                                ? document.uri
                                : `file://${note.span.path.value}`,
                        range: { start, end },
                    },
                };
            }),
            range: { start, end },
            source: "wipple",
        };

        return [lspDiagnostic];
    });

connection.onRequest(SemanticTokensDeltaRequest.type, (params) => {
    console.warn("DELTA REQUEST");

    const builder = new SemanticTokensBuilder();
    builder.previousResult(params.previousResultId);

    const s = storage[params.textDocument.uri];
    if (!s) return builder.build();
    const [path, document, program] = s;

    let tokens: [Span, SemanticTokenTypes][] = [];

    const add = (span: Span, type: SemanticTokenTypes) => {
        if (span.path.value !== path) return;

        tokens.push([span, type]);
    };

    const traverse = (expr: Expression) => {
        switch (expr.ty.type) {
            case "Function":
                add(expr.span, SemanticTokenTypes.function);
                break;
            default:
                break;
        }

        switch (expr.kind.type) {
            case "Marker":
                break;
            case "Variable":
                break;
            case "Constant":
                break;
            case "Text":
                break;
            case "Number":
                break;
            case "Block":
                for (const inner of expr.kind.value) {
                    traverse(inner);
                }
                break;
            case "Call":
                traverse(expr.kind.value[0]);
                traverse(expr.kind.value[1]);
                break;
            case "Function":
                traverse(expr.kind.value[1]);
                break;
            case "When":
                traverse(expr.kind.value[0]);
                for (const arm of expr.kind.value[1]) {
                    traverse(arm.body);
                }
                break;
            case "External":
                for (const inner of expr.kind.value[2]) {
                    traverse(inner);
                }
                break;
            case "Initialize":
                traverse(expr.kind.value[1]);
                break;
            case "Structure":
                for (const inner of expr.kind.value) {
                    traverse(inner);
                }
                break;
            case "Variant":
                for (const inner of expr.kind.value[1]) {
                    traverse(inner);
                }
                break;
            case "Return":
                traverse(expr.kind.value);
                break;
            case "Loop":
                traverse(expr.kind.value);
                break;
            case "Break":
                traverse(expr.kind.value);
                break;
            case "Continue":
                break;
            case "Tuple":
                for (const inner of expr.kind.value) {
                    traverse(inner);
                }
                break;
        }
    };

    for (const expr of program.body) {
        traverse(expr);
    }

    const operators: string[] = [];
    for (const operator of Object.values(program.declarations.operators)) {
        operators.push(operator.template.toString());

        const template = program.declarations.templates[operator.template];

        add(template.span, SemanticTokenTypes.operator);

        for (const use of template.uses) {
            add(use, SemanticTokenTypes.operator);
        }
    }

    for (const [id, template] of Object.entries(program.declarations.templates)) {
        if (operators.includes(id)) continue;

        const type = template.attributes.keyword
            ? SemanticTokenTypes.keyword
            : SemanticTokenTypes.macro;

        add(template.span, type);

        for (const use of template.uses) {
            add(use, type);
        }
    }

    for (const type of Object.values(program.declarations.types)) {
        add(type.span, SemanticTokenTypes.type);

        for (const use of type.uses) {
            add(use, SemanticTokenTypes.type);
        }
    }

    for (const param of Object.values(program.declarations.type_parameters)) {
        add(param.span, SemanticTokenTypes.typeParameter);

        for (const use of param.uses) {
            add(use, SemanticTokenTypes.typeParameter);
        }
    }

    for (const trait of Object.values(program.declarations.traits)) {
        add(trait.span, SemanticTokenTypes.interface);

        for (const use of trait.uses) {
            add(use, SemanticTokenTypes.interface);
        }
    }

    for (const [_file, _genericId, constant] of Object.values(
        program.declarations.monomorphized_constants
    )) {
        add(constant.span, SemanticTokenTypes.variable);

        for (const use of constant.uses) {
            add(use, SemanticTokenTypes.variable);
        }
    }

    for (const variable of Object.values(program.declarations.variables)) {
        add(variable.span, SemanticTokenTypes.variable);

        for (const use of variable.uses) {
            add(use, SemanticTokenTypes.variable);
        }
    }

    tokens.sort(([left], [right]) => left.start - right.start);

    tokens = _.uniqBy(tokens, ([span]) => span.start);

    for (const [span, type] of tokens) {
        const start = document.positionAt(span.start);
        const end = document.positionAt(span.end);

        builder.push(
            start.line,
            start.character,
            end.character - start.character,
            tokenTypes.indexOf(type),
            0
        );
    }

    return builder.buildEdits();
});

documents.listen(connection);
connection.listen();
