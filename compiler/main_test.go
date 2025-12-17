package main_test

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	"wipple/database"
	"wipple/driver"
	"wipple/nodes/file"
	"wipple/syntax"

	"github.com/gkampitakis/go-snaps/snaps"
)

func TestFiles(t *testing.T) {
	cwd, err := os.Getwd()
	if err != nil {
		panic(err)
	}

	cwd = filepath.Dir(cwd)

	testDir := filepath.Join(cwd, "tests")

	entries, err := os.ReadDir(testDir)
	if err != nil {
		panic(err)
	}

	for _, entry := range entries {
		if entry.IsDir() || filepath.Ext(entry.Name()) != ".wipple" {
			continue
		}

		t.Run(entry.Name(), func(t *testing.T) {
			path := filepath.Join(testDir, entry.Name())
			source, err := os.ReadFile(path)
			if err != nil {
				panic(err)
			}

			filter := func(node database.Node) bool {
				return true
			}

			db, root := driver.MakeRoot()

			f, syntaxError := syntax.Parse(db, entry.Name(), string(source), file.ParseFile)
			if syntaxError != nil {
				panic(syntaxError)
			}

			driver.Compile(db, root, []*file.FileNode{f})

			var buf bytes.Buffer
			db.Write(&buf, nil)
			driver.WriteFeedback(db, filter, nil, &buf)

			snaps.WithConfig(snaps.Dir(filepath.Join(testDir, "__snapshots__")), snaps.Filename(entry.Name())).MatchStandaloneSnapshot(t, buf.String())
		})
	}
}
