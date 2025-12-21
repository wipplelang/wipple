package lsp

import (
	"net/url"
	"path/filepath"
	"slices"
	"strings"
	"unicode"
	"wipple/database"
	"wipple/driver"
	"wipple/feedback"
	"wipple/nodes/file"
	"wipple/queries"
	"wipple/syntax"
	"wipple/typecheck"
	"wipple/vendored/semtok"

	"github.com/tliron/commonlog"
	_ "github.com/tliron/commonlog/simple"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
	"github.com/tliron/glsp/server"
)

var (
	handler       protocol.Handler
	workspacePath = ""
	sources       = map[protocol.DocumentUri]string{}
	dbs           = map[protocol.DocumentUri]*database.Db{}

	tokenTypes = []string{"type", "interface", "typeParameter", "function"}
)

func Run() error {
	commonlog.Configure(2, nil)

	handler = protocol.Handler{
		Initialize:                     initialize,
		Shutdown:                       shutdown,
		SetTrace:                       setTrace,
		TextDocumentDidOpen:            didOpen,
		TextDocumentDidChange:          didChange,
		TextDocumentHover:              hover,
		TextDocumentDefinition:         definition,
		TextDocumentReferences:         references,
		TextDocumentDocumentHighlight:  documentHighlight,
		TextDocumentSemanticTokensFull: semanticTokens,
		TextDocumentFormatting:         format,
	}

	server := server.NewServer(&handler, "wipple", false)

	return server.RunStdio()
}

func initialize(context *glsp.Context, params *protocol.InitializeParams) (any, error) {
	if len(params.WorkspaceFolders) > 0 {
		workspacePath = path(params.WorkspaceFolders[0].URI)
	}

	capabilities := handler.CreateServerCapabilities()

	openClose := true
	change := protocol.TextDocumentSyncKindFull
	capabilities.TextDocumentSync = &protocol.TextDocumentSyncOptions{
		OpenClose: &openClose,
		Change:    &change,
	}

	capabilities.SemanticTokensProvider = &protocol.SemanticTokensOptions{
		Legend: protocol.SemanticTokensLegend{
			TokenTypes: tokenTypes,
		},
		Full: true,
	}

	capabilities.HoverProvider = true

	capabilities.DocumentHighlightProvider = true

	return protocol.InitializeResult{Capabilities: capabilities}, nil
}

func shutdown(context *glsp.Context) error {
	protocol.SetTraceValue(protocol.TraceValueOff)
	return nil
}

func setTrace(context *glsp.Context, params *protocol.SetTraceParams) error {
	protocol.SetTraceValue(params.Value)
	return nil
}

func didOpen(context *glsp.Context, params *protocol.DidOpenTextDocumentParams) error {
	sources[params.TextDocument.URI] = params.TextDocument.Text

	update(context, params.TextDocument.URI)

	return nil
}

func didChange(context *glsp.Context, params *protocol.DidChangeTextDocumentParams) error {
	source := params.ContentChanges[0].(protocol.TextDocumentContentChangeEventWhole).Text
	sources[params.TextDocument.URI] = source

	update(context, params.TextDocument.URI)

	return nil
}

func update(context *glsp.Context, uri protocol.DocumentUri) {
	source := sources[uri]

	filter := nodeFilter(uri)

	db, root := driver.MakeRoot()

	// TODO: Support multiple files
	f, err := syntax.Parse(db, path(uri), source, file.ParseFile)
	if err == nil {
		driver.Compile(db, root, []*file.FileNode{f})
	} else {
		// Syntax error is written to `db` and will be shown in diagnostics below
	}

	diagnostics := addFeedback(db, filter)

	context.Notify("textDocument/publishDiagnostics", &protocol.PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diagnostics,
	})

	dbs[uri] = db
}

func semanticTokens(context *glsp.Context, params *protocol.SemanticTokensParams) (*protocol.SemanticTokens, error) {
	db, ok := dbs[params.TextDocument.URI]
	if !ok {
		return nil, nil
	}

	tokens := addSemanticTokens(db, params.TextDocument.URI)
	return &tokens, nil
}

func hover(context *glsp.Context, params *protocol.HoverParams) (*protocol.Hover, error) {
	db, ok := dbs[params.TextDocument.URI]
	if !ok {
		return nil, nil
	}

	hover := getHover(db, params.TextDocument.URI, params.Position)
	if hover == nil {
		return nil, nil
	}

	return hover, nil
}

func definition(context *glsp.Context, params *protocol.DefinitionParams) (any, error) {
	db, ok := dbs[params.TextDocument.URI]
	if !ok {
		return nil, nil
	}

	nodeAtPosition := getNodeAtPosition(db, params.TextDocument.URI, params.Position)
	if nodeAtPosition == nil {
		return nil, nil
	}

	filter := nodeFilter(params.TextDocument.URI)

	var nodes []database.Node
	queries.Definitions(db, nodeAtPosition, filter, func(ns []database.Node) {
		nodes = append(nodes, ns...)
	})
	queries.References(db, nodeAtPosition, filter, func(ns []database.Node) {
		nodes = append(nodes, ns...)
	})

	return getLocations(nodes, params.TextDocument.URI), nil
}

func references(context *glsp.Context, params *protocol.ReferenceParams) ([]protocol.Location, error) {
	db, ok := dbs[params.TextDocument.URI]
	if !ok {
		return nil, nil
	}

	nodeAtPosition := getNodeAtPosition(db, params.TextDocument.URI, params.Position)
	if nodeAtPosition == nil {
		return nil, nil
	}

	filter := nodeFilter(params.TextDocument.URI)

	var references []database.Node
	queries.References(db, nodeAtPosition, filter, func(r []database.Node) {
		references = append(references, r...)
	})

	return getLocations(references, params.TextDocument.URI), nil
}

func documentHighlight(context *glsp.Context, params *protocol.DocumentHighlightParams) ([]protocol.DocumentHighlight, error) {
	db, ok := dbs[params.TextDocument.URI]
	if !ok {
		return nil, nil
	}

	highlights := getRelated(db, params.TextDocument.URI, params.Position)
	return highlights, nil
}

func format(context *glsp.Context, params *protocol.DocumentFormattingParams) ([]protocol.TextEdit, error) {
	source := sources[params.TextDocument.URI]

	formatted, err := syntax.Format(source)
	if err != nil {
		return nil, nil
	}

	textEdit := protocol.TextEdit{
		Range: protocol.Range{
			Start: protocol.Position{Line: 0, Character: 0},
			End:   protocol.Position{Line: uint32(strings.Count(source, "\n") + 1), Character: 0},
		},
		NewText: formatted + "\n",
	}

	return []protocol.TextEdit{textEdit}, nil
}

func path(uri protocol.DocumentUri) string {
	parsed, err := url.Parse(string(uri))
	if err != nil {
		return ""
	}

	path, err := filepath.Rel(workspacePath, parsed.Path)
	if err != nil {
		return ""
	}

	return path
}

func convertSpan(span database.Span) protocol.Range {
	return protocol.Range{
		Start: protocol.Position{
			Line:      uint32(span.Start.Line - 1),
			Character: uint32(span.Start.Column - 1),
		},
		End: protocol.Position{
			Line:      uint32(span.End.Line - 1),
			Character: uint32(span.End.Column),
		},
	}
}

func nodeFilter(uri protocol.DocumentUri) func(node database.Node) bool {
	path := path(uri)

	return func(node database.Node) bool {
		return database.GetSpanFact(node).Path == path
	}
}

func addFeedback(db *database.Db, filter func(node database.Node) bool) []protocol.Diagnostic {
	seenFeedback := map[database.Node]map[string]struct{}{}
	items := feedback.Collect(db, filter, func(item feedback.FeedbackItem) bool {
		nodeFeedback, ok := seenFeedback[item.On[0]]
		if !ok {
			nodeFeedback = map[string]struct{}{}
			seenFeedback[item.On[0]] = nodeFeedback
		}

		if _, ok := nodeFeedback[item.Id]; ok {
			return false
		}
		nodeFeedback[item.Id] = struct{}{}

		return true
	})

	diagnostics := []protocol.Diagnostic{}
	for _, item := range items {
		diagnosticSeverity := protocol.DiagnosticSeverityInformation
		diagnosticSource := "wipple"

		diagnostics = append(diagnostics, protocol.Diagnostic{
			Severity: &diagnosticSeverity,
			Range:    convertSpan(database.GetSpanFact(item.On[0])),
			Message:  item.String(),
			Source:   &diagnosticSource,
		})
	}

	return diagnostics
}

func addSemanticTokens(db *database.Db, uri protocol.DocumentUri) protocol.SemanticTokens {
	filter := nodeFilter(uri)

	tokens := map[database.Span]semtok.Type{}
	database.ContainsNode(db, func(node database.Node) (struct{}, bool) {
		if !filter(node) {
			return struct{}{}, false
		}

		span := database.GetSpanFact(node)

		// Don't highlight across whitespace
		if span.Start.Line != span.End.Line || strings.ContainsFunc(span.Source, unicode.IsSpace) {
			return struct{}{}, false
		}

		queries.HighlightType(db, node, filter, func(struct{}) {
			tokens[span] = semtok.TokType
		})

		queries.HighlightTrait(db, node, filter, func(struct{}) {
			tokens[span] = semtok.TokNamespace
		})

		queries.HighlightTypeParameter(db, node, filter, func(struct{}) {
			tokens[span] = semtok.TokTypeParam
		})

		queries.HighlightFunction(db, node, filter, func(struct{}) {
			tokens[span] = semtok.TokFunction
		})

		return struct{}{}, false
	})

	sortedTokens := make([]semtok.Token, 0, len(tokens))
	for span, tokenType := range tokens {
		sortedTokens = append(sortedTokens, semtok.Token{
			Line:  uint32(span.Start.Line - 1),
			Start: uint32(span.Start.Column - 1),
			Len:   uint32(span.End.Index - span.Start.Index),
			Type:  tokenType,
		})
	}
	slices.SortStableFunc(sortedTokens, func(left semtok.Token, right semtok.Token) int {
		if left.Line != right.Line {
			return int(left.Line) - int(right.Line)
		}

		return int(left.Start) - int(right.Start)
	})

	encoded := semtok.Encode(sortedTokens, nil, nil)
	return protocol.SemanticTokens{Data: encoded}
}

func getHover(db *database.Db, uri protocol.DocumentUri, position protocol.Position) *protocol.Hover {
	filter := nodeFilter(uri)

	nodeAtPosition := getNodeAtPosition(db, uri, position)
	if nodeAtPosition == nil {
		return nil
	}

	contents := []any{}
	queries.Type(db, nodeAtPosition, filter, func(ty *typecheck.ConstructedType) {
		contents = append(contents, protocol.MarkedStringStruct{
			Language: "wipple",
			Value:    typecheck.DisplayType(ty, true),
		})
	})

	queries.Comments(db, nodeAtPosition, filter, func(data queries.CommentsData) {
		if data.Node != nodeAtPosition {
			return
		}

		render := feedback.NewRender(db)
		render.WriteComments(data)
		documentation := strings.TrimSpace(render.Finish())

		if documentation != "" {
			contents = append(contents, documentation)
		}
	})

	hoverRange := convertSpan(database.GetSpanFact(nodeAtPosition))
	return &protocol.Hover{
		Range:    &hoverRange,
		Contents: contents,
	}
}

func getRelated(db *database.Db, uri protocol.DocumentUri, position protocol.Position) []protocol.DocumentHighlight {
	filter := nodeFilter(uri)

	nodeAtPosition := getNodeAtPosition(db, uri, position)
	if nodeAtPosition == nil {
		return nil
	}

	highlights := []protocol.DocumentHighlight{
		{Range: convertSpan(database.GetSpanFact(nodeAtPosition))},
	}

	queries.Related(db, nodeAtPosition, filter, func(related database.Node) {
		highlights = append(highlights, protocol.DocumentHighlight{
			Range: convertSpan(database.GetSpanFact(related)),
		})
	})

	return highlights
}

func getLocations(nodes []database.Node, uri protocol.DocumentUri) []protocol.Location {
	path := path(uri)

	seen := map[database.Span]struct{}{}
	locations := make([]protocol.Location, 0, len(nodes))
	for _, definition := range nodes {
		span := database.GetSpanFact(definition)
		if span.Path != path {
			continue // ignore definitions from other files
		}

		if _, ok := seen[span]; ok {
			continue
		}
		seen[span] = struct{}{}

		locations = append(locations, protocol.Location{
			URI:   uri,
			Range: convertSpan(span),
		})
	}

	return locations
}

func getNodeAtPosition(db *database.Db, uri protocol.DocumentUri, position protocol.Position) database.Node {
	filter := nodeFilter(uri)

	type match struct {
		length uint32
		node   database.Node
	}

	matches := []match{}
	database.ContainsNode(db, func(node database.Node) (struct{}, bool) {
		if !filter(node) {
			return struct{}{}, false
		}

		r := convertSpan(database.GetSpanFact(node))

		if r.Start.Line == position.Line &&
			r.Start.Character <= position.Character &&
			r.End.Line == position.Line &&
			r.End.Character >= position.Character {
			matches = append(matches, match{
				length: r.End.Character - r.Start.Character,
				node:   node,
			})
		}

		return struct{}{}, false
	})

	slices.SortFunc(matches, func(left match, right match) int {
		return int(left.length) - int(right.length)
	})

	if len(matches) == 0 {
		return nil
	}

	return matches[0].node
}
