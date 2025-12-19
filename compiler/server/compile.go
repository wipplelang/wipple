package server

import (
	"fmt"
	"slices"
	"sync"
	"wipple/codegen"
	"wipple/colors"
	"wipple/database"
	"wipple/driver"
	"wipple/feedback"
	"wipple/nodes/file"
	"wipple/syntax"
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
	*CompileResponseSuccess
	*CompileResponseFailure
}

type CompileResponseSuccess struct {
	Executable string `json:"executable"`
}

type CompileResponseFailure struct {
	Diagnostics []ResponseDiagnostic `json:"diagnostics"`
}

type ResponseDiagnostic struct {
	Location database.Span `json:"location"`
	Message  string        `json:"message"`
}

type ResponseDiagnosticLocation struct {
	Line   int `json:"line"`
	Column int `json:"column"`
	Index  int `json:"index"`
}

func (request *CompileRequest) handle() (*CompileResponse, error) {
	var files []File
	if len(request.Files) > 0 {
		files = request.Files
	} else {
		files = []File{{Path: "input", Code: request.Code}}
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
		if database.IsHiddenNode(item.On) || !filter(item.On) {
			return false
		}

		if slices.Contains(seenFeedback[item.On], item.Id) {
			return false
		}

		seenFeedback[item.On] = append(seenFeedback[item.On], item.Id)

		return true
	})

	if len(feedbackItems) > 0 {
		responseDiagnostics := make([]ResponseDiagnostic, 0, len(feedbackItems))
		for _, item := range feedbackItems {
			span := database.GetSpanFact(item.On)

			responseDiagnostics = append(responseDiagnostics, ResponseDiagnostic{
				Location: span,
				Message:  item.String(),
			})
		}

		return &CompileResponse{
			CompileResponseFailure: &CompileResponseFailure{Diagnostics: responseDiagnostics},
		}, nil
	} else {
		codegen := codegen.NewCodegen(db, "index.js", codegen.Options{
			Module:    true,
			Prelude:   Prelude,
			Sourcemap: true,
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

		return &CompileResponse{
			CompileResponseSuccess: &CompileResponseSuccess{Executable: script},
		}, nil
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
