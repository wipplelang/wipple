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
	"wipple/typecheck"
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
	Locations []*ResponseDiagnosticLocation `json:"locations"`
	Lines     []*ResponseDiagnosticLine     `json:"lines"`
	Message   string                        `json:"message"`
}

type ResponseDiagnosticLocation struct {
	Start int `json:"start"`
	End   int `json:"end"`
	Group int `json:"group"`
	node  database.Node
}

type ResponseDiagnosticLine struct {
	Path      string                        `json:"path"`
	Source    string                        `json:"source"`
	Locations []*ResponseDiagnosticLocation `json:"locations"`
	start     int
	end       int
	offset    int
	node      database.Node
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
			slices.SortStableFunc(item.On[1:], func(left database.Node, right database.Node) int {
				return database.CompareSpans(database.GetSpanFact(left), database.GetSpanFact(right))
			})
			item.On = slices.CompactFunc(item.On, func(left database.Node, right database.Node) bool {
				return database.HaveEqualSpans(left, right)
			})

			groups := map[*typecheck.Group]int{}
			locations, lines := collectLines(db, item.On, groups)

			message := database.WrappingDisplayNode(func(node database.Node, source string) string {
				if fact, ok := database.GetFact[typecheck.TypedFact](node); ok {
					if groupIndex, ok := groups[fact.Group]; ok {
						return fmt.Sprintf("<code data-group=\"%d\">%s</code>", groupIndex, source)
					}
				}

				return "`" + source + "`"
			}, func() string {
				return item.String()
			})

			responseDiagnostics = append(responseDiagnostics, ResponseDiagnostic{
				Locations: locations,
				Lines:     lines,
				Message:   message,
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

func collectLines(db *database.Db, nodes []database.Node, groups map[*typecheck.Group]int) ([]*ResponseDiagnosticLocation, []*ResponseDiagnosticLine) {
	var locations []*ResponseDiagnosticLocation
	var lines []*ResponseDiagnosticLine
	for _, node := range nodes {
		groupIndex := -1
		fact, ok := database.GetFact[typecheck.TypedFact](node)
		if ok && fact.Group != nil {
			if _, ok := groups[fact.Group]; !ok {
				groups[fact.Group] = len(groups)
			}

			groupIndex = groups[fact.Group]
		}

		span := database.GetSpanFact(node)

		if span.Path == defaultPath {
			location := ResponseDiagnosticLocation{
				Start: span.Start.Index,
				End:   span.End.Index,
				Group: groupIndex,
				node:  node,
			}

			if !slices.ContainsFunc(locations, func(other *ResponseDiagnosticLocation) bool {
				return other.Start == location.Start && other.End == location.End
			}) {
				locations = append(locations, &location)
			}

			continue
		}

		getLocation := func(offset int) *ResponseDiagnosticLocation {
			return &ResponseDiagnosticLocation{
				Start: span.Start.Column - 1 + offset,
				End:   span.End.Column + offset,
				Group: groupIndex,
				node:  node,
			}
		}

		if index := slices.IndexFunc(lines, func(line *ResponseDiagnosticLine) bool {
			return line.Path == span.Path && span.Start.Line >= line.start && span.End.Line <= line.end
		}); index != -1 {
			existing := lines[index]
			location := getLocation(existing.offset)

			if !slices.ContainsFunc(existing.Locations, func(other *ResponseDiagnosticLocation) bool {
				return other.Start == location.Start && other.End == location.End
			}) {
				existing.Locations = append(existing.Locations, location)
			}

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

		lines = append(lines, &ResponseDiagnosticLine{
			Path:      span.Path,
			Source:    strings.Join(sourceLines, "\n"),
			Locations: []*ResponseDiagnosticLocation{getLocation(offset)},
			start:     span.Start.Line,
			end:       span.End.Line,
			offset:    offset,
			node:      node,
		})
	}

	locations = removeDuplicateLocations(locations)

	for _, line := range lines {
		line.Locations = removeDuplicateLocations(line.Locations)
	}

	// Delete lines that have no groups
	lines = slices.DeleteFunc(lines, func(line *ResponseDiagnosticLine) bool {
		for _, location := range line.Locations {
			if location.Group != -1 {
				return false
			}
		}

		return true
	})

	return locations, lines
}

func removeDuplicateLocations(locations []*ResponseDiagnosticLocation) []*ResponseDiagnosticLocation {
	if len(locations) == 0 {
		return nil
	}

	slices.SortStableFunc(locations, func(left *ResponseDiagnosticLocation, right *ResponseDiagnosticLocation) int {
		// Prefer non-hidden nodes
		if !database.IsHiddenNode(left.node) && database.IsHiddenNode(right.node) {
			return -1
		} else if database.IsHiddenNode(left.node) && !database.IsHiddenNode(right.node) {
			return 1
		}

		return (left.End - left.Start) - (right.End - right.Start)
	})

	locations = slices.CompactFunc(locations, func(left *ResponseDiagnosticLocation, right *ResponseDiagnosticLocation) bool {
		return left.Start == right.Start && left.End == right.End
	})

	return locations
}
