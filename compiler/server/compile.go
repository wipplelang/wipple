package server

import (
	"fmt"
	"slices"
	"strings"
	"sync"
	"wipple/codegen"
	"wipple/colors"
	"wipple/database"
	"wipple/driver"
	"wipple/feedback"
	"wipple/nodes/file"
	"wipple/syntax"
	"wipple/visit"
)

type CompileRequest struct {
	InputMetadata
	Code  string `json:"code,omitempty"`
	Files []File `json:"files,omitempty"`
}

type File struct {
	Path string `json:"path"`
	Code string `json:"code"`
}

type CompileResponse struct {
	Executable  string               `json:"executable,omitempty"`
	Diagnostics []ResponseDiagnostic `json:"diagnostics,omitempty"`
}

type ResponseDiagnostic struct {
	Locations      []database.Span          `json:"locations"`
	PrimaryLines   []ResponseDiagnosticLine `json:"primaryLines"`
	Message        string                   `json:"message"`
	SecondaryLines []ResponseDiagnosticLine `json:"secondaryLines"`
}

type ResponseDiagnosticLocation struct {
	Line   int `json:"line"`
	Column int `json:"column"`
	Index  int `json:"index"`
}

type ResponseDiagnosticLine struct {
	Path   string `json:"path"`
	number int
	Source string `json:"source"`
	Start  int    `json:"start"`
	End    int    `json:"end"`
	node   database.Node
}

const defaultPath = "input"

func (request *CompileRequest) handle() (*CompileResponse, error) {
	var files []File
	if len(request.Files) > 0 {
		files = request.Files
	} else {
		files = []File{{Path: defaultPath, Code: request.Code}}
	}

	db, root, err := compile(files, request.Library)
	if err != nil {
		return nil, err
	}

	filter := func(node database.Node) bool {
		return true
	}

	seenFeedback := map[database.Node][]string{}
	feedbackItems := feedback.Collect(db, filter, func(item feedback.FeedbackItem) bool {
		if database.IsHiddenNode(item.On[0]) || !filter(item.On[0]) {
			return false
		}

		if slices.Contains(seenFeedback[item.On[0]], item.Id) {
			return false
		}

		seenFeedback[item.On[0]] = append(seenFeedback[item.On[0]], item.Id)

		return true
	})

	if len(feedbackItems) > 0 {
		responseDiagnostics := make([]ResponseDiagnostic, 0, len(feedbackItems))
		for _, item := range feedbackItems {
			database.RemoveOverlappingNodes(item.On[1:])
			slices.SortFunc(item.On[1:], func(left database.Node, right database.Node) int {
				return database.CompareSpans(database.GetSpanFact(left), database.GetSpanFact(right))
			})

			spans := make([]database.Span, len(item.On))
			for i, node := range item.On {
				spans[i] = database.GetSpanFact(node)
			}

			getPriority := func(node database.Node) int {
				definition, isDefinition := database.GetFact[visit.DefinedFact](node)
				if !isDefinition {
					return 0
				}

				switch definition.Definition.(type) {
				case *visit.ConstantDefinition:
					return -3
				case *visit.TraitDefinition:
					return -2
				case *visit.InstanceDefinition:
					return -1
				default:
					return 0
				}
			}

			priorities := map[database.Node]int{}
			highestPriority := 0
			for _, node := range item.On[1:] {
				priority := getPriority(node)
				priorities[node] = priority
				if priority < highestPriority {
					highestPriority = priority
				}
			}

			primaryLines, secondaryLines := collectLines(db, item.On[1:])

			responseDiagnostics = append(responseDiagnostics, ResponseDiagnostic{
				Locations:      spans,
				PrimaryLines:   primaryLines,
				SecondaryLines: secondaryLines,
				Message:        item.String(),
			})
		}

		return &CompileResponse{Diagnostics: responseDiagnostics}, nil
	} else {
		codegen := codegen.NewCodegen(db, "index.js", codegen.Options{
			Module:    true,
			Prelude:   Prelude,
			Sourcemap: true,
			Optimize:  true,
		})

		files := make([]database.Node, 0, len(root.Files))
		for _, file := range root.Files {
			files = append(files, file)
		}

		var script string
		colors.WithoutColor(func() {
			db := db
			_ = db
			script, err = codegen.String(root, files)
		})
		if err != nil {
			return nil, err
		}

		return &CompileResponse{Executable: script}, nil
	}
}

var (
	cacheMutex = sync.RWMutex{}
	cache      = map[string]struct {
		db   *database.Db
		root *driver.RootNode
	}{}
)

func compile(files []File, libraryName string) (*database.Db, *driver.RootNode, error) {
	var db *database.Db
	var root *driver.RootNode
	if libraryName != "" {
		var err error
		db, root, err = compileLibrary(libraryName)
		if err != nil {
			return nil, nil, err
		}
	} else {
		db, root = driver.MakeRoot()
	}

	var driverFiles []*file.FileNode
	hasSyntaxError := false
	for _, f := range files {
		driverFile, err := syntax.Parse(db, f.Path, f.Code, file.ParseFile)
		if err == nil {
			driverFiles = append(driverFiles, driverFile)
		} else {
			// Syntax error is written to `db` and will be shown in diagnostics below
			hasSyntaxError = true
		}
	}

	if !hasSyntaxError {
		driver.Compile(db, root, driverFiles)
	}

	return db, root, nil
}

func compileLibrary(name string) (*database.Db, *driver.RootNode, error) {
	libraryEntry, cached, err := fetchLibrary(name)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to fetch library %q: %w", name, err)
	}

	if cached {
		cacheMutex.RLock()
		cached, ok := cache[name]
		cacheMutex.RUnlock()
		if ok {
			db := database.NewDb(cached.db)
			root := driver.CloneRoot(cached.root)
			db.Register(root)

			return db, root, nil
		}
	}

	db, root, err := compile(libraryEntry.Files, libraryEntry.Metadata.Library)
	if err != nil {
		return nil, nil, fmt.Errorf("failed to compile library %q", name)
	}

	cacheMutex.Lock()
	cache[name] = struct {
		db   *database.Db
		root *driver.RootNode
	}{
		db:   db,
		root: root,
	}
	cacheMutex.Unlock()

	db = database.NewDb(db)
	root = driver.CloneRoot(root)
	db.Register(root)

	return db, root, nil
}

func collectLines(db *database.Db, nodes []database.Node) ([]ResponseDiagnosticLine, []ResponseDiagnosticLine) {
	var lines []ResponseDiagnosticLine
	for _, node := range nodes {
		span := database.GetSpanFact(node)

		if span.Path == defaultPath || slices.ContainsFunc(lines, func(line ResponseDiagnosticLine) bool {
			return line.Path == span.Path && line.number == span.Start.Line
		}) {
			continue
		}

		source, ok := database.ContainsNode(db, func(node database.Node) (string, bool) {
			if _, ok := node.(*file.FileNode); !ok {
				return "", false
			}

			fileSpan := database.GetSpanFact(node)
			if fileSpan.Path == span.Path {
				return fileSpan.Source, true
			}

			return "", false
		})

		if !ok {
			continue
		}

		sourceLines := strings.Split(source, "\n")[span.Start.Line-1 : span.End.Line]
		trimmedLines := 0
		for {
			if strings.HasPrefix(strings.TrimLeft(sourceLines[0], " "), "--") {
				sourceLines = sourceLines[1:]
				trimmedLines++
			} else {
				break
			}
		}

		offset := 0
		for i := 0; i < span.End.Line-span.Start.Line-trimmedLines; i++ {
			offset += len(sourceLines[i]) + 1
		}

		lines = append(lines, ResponseDiagnosticLine{
			Path:   span.Path,
			number: span.Start.Line,
			Source: strings.Join(sourceLines, "\n"),
			Start:  span.Start.Column - 1 + offset,
			End:    span.End.Column + offset,
			node:   node,
		})
	}

	getPriority := func(node database.Node) int {
		span := database.GetSpanFact(node)

		// Get the definition on this line
		if definition, ok := database.ContainsFact(db, func(definitionNode database.Node, fact visit.DefinedFact) (visit.Definition, bool) {
			definitionSpan := database.GetSpanFact(definitionNode)

			if definitionSpan.Path == span.Path && span.Start.Line >= definitionSpan.Start.Line && span.End.Line <= definitionSpan.End.Line {
				return fact.Definition, true
			}

			return nil, false
		}); ok {
			switch definition.(type) {
			case *visit.TraitDefinition:
				return 0
			case *visit.InstanceDefinition:
				return 1
			case *visit.ConstantDefinition:
				return 2
			}
		}

		return 3
	}

	priorities := map[database.Node]int{}
	highestPriority := 0
	for _, line := range lines {
		priority := getPriority(line.node)
		priorities[line.node] = priority
		if priority > highestPriority {
			highestPriority = priority
		}
	}

	var primaryLines []ResponseDiagnosticLine
	var secondaryLines []ResponseDiagnosticLine
	for _, line := range lines {
		if priorities[line.node] == highestPriority {
			primaryLines = append(primaryLines, line)
		} else {
			secondaryLines = append(secondaryLines, line)
		}
	}

	return primaryLines, secondaryLines
}
