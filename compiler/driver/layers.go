package driver

import (
	"fmt"
	"os"
	"path/filepath"
	"wipple/database"
	"wipple/nodes/file"
	"wipple/syntax"
)

type Layer struct {
	Name  string
	Files []*file.FileNode
}

func ReadFile(db *database.Db, path string) (*file.FileNode, error) {
	source, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	file, _ := syntax.Parse(db, path, string(source), file.ParseFile)
	return file, nil
}

func ReadLayers(db *database.Db, path string, cwd string) (Layer, error) {
	if cwd != "" {
		var err error
		path, err = filepath.Rel(cwd, path)
		if err != nil {
			return Layer{}, err
		}
	} else if !filepath.IsAbs(path) {
		return Layer{}, fmt.Errorf("layer path must be absolute")
	}

	entries, err := os.ReadDir(path)
	if err != nil {
		return Layer{}, err
	}

	files := make([]*file.FileNode, 0, len(entries))
	for _, entry := range entries {
		if !entry.IsDir() && filepath.Ext(entry.Name()) == ".wipple" {
			file, err := ReadFile(db, filepath.Join(path, entry.Name()))
			if err != nil {
				return Layer{}, err
			}

			if file != nil {
				files = append(files, file)
			}
		}
	}

	return Layer{Name: path, Files: files}, nil
}
