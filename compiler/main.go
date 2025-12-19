package main

import (
	_ "embed"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime/pprof"
	"slices"
	"strconv"
	"strings"
	"time"

	"wipple/codegen"
	"wipple/colors"
	"wipple/database"
	"wipple/driver"
	"wipple/lsp"
	"wipple/nodes/file"
	"wipple/server"
	"wipple/syntax"

	"github.com/alecthomas/kong"
	"github.com/fatih/color"
)

//go:embed runtime/node-prelude.js
var nodePrelude string

//go:embed runtime/runtime.js
var runtime string

type Context struct{}

type CompileCmd struct {
	Lib            []string `type:"path"`
	Facts          bool
	Output         string   `short:"o" type:"path"`
	FilterLines    []string `short:"l"`
	FilterFeedback []string
	Paths          []string `arg:"" name:"path" type:"path"`
	Profile        string   `type:"path"`
}

func (cmd *CompileCmd) Run(ctx *Context) error {
	output, _, err := compile(cmd, false)
	fmt.Print(output)
	if err != nil {
		return err
	}

	return nil
}

type RunCmd CompileCmd

func (cmd *RunCmd) Run(ctx *Context) error {
	output, script, err := compile((*CompileCmd)(cmd), true)
	fmt.Print(output)
	if err != nil {
		return err
	}

	fmt.Println()

	outputPath := cmd.Output
	if outputPath == "" {

		tmp, err := os.CreateTemp(os.TempDir(), "wipple-*.js")
		if err != nil {
			return err
		}
		defer os.Remove(tmp.Name())

		_, err = tmp.WriteString(script)
		if err != nil {
			return err
		}

		err = tmp.Close()
		if err != nil {
			return err
		}

		outputPath = tmp.Name()
	}

	command := exec.Command("/usr/bin/env", "node", "--enable-source-maps", outputPath)
	command.Stdin = os.Stdin
	command.Stdout = os.Stdout
	command.Stderr = os.Stderr
	err = command.Run()
	if err != nil {
		return err
	}

	return nil
}

type FormatCmd struct {
	Path string `arg:"" type:"path" required:""`
}

func (cmd *FormatCmd) Run(ctx *Context) error {
	return format(cmd)
}

type LspCmd struct {
	Stdio bool
}

func (cmd *LspCmd) Run(ctx *Context) error {
	if !cmd.Stdio {
		return fmt.Errorf("expected --stdio")
	}

	database.LspEnabled = true
	return lsp.Run()
}

type ServerCmd struct {
	Lambda bool `cmd:""`
}

func (cmd *ServerCmd) Run(ctx *Context) error {
	color.NoColor = true
	database.LspEnabled = true
	server.Prelude = runtime
	return server.Run(cmd.Lambda)
}

var cli struct {
	Compile CompileCmd `cmd:""`
	Run     RunCmd     `cmd:""`
	Format  FormatCmd  `cmd:""`
	Lsp     LspCmd     `cmd:""`
	Server  ServerCmd  `cmd:""`
}

func main() {
	// Default to server if running as Lambda function
	if os.Getenv("LAMBDA_TASK_ROOT") != "" {
		(&ServerCmd{Lambda: true}).Run(&Context{})
		return
	}

	ctx := kong.Parse(&cli)
	err := ctx.Run(&Context{})
	ctx.FatalIfErrorf(err)
}

func compile(cmd *CompileCmd, run bool) (string, string, error) {
	if cmd.Profile != "" {
		cpu, err := os.Create(filepath.Join(cmd.Profile, "cpu.pprof"))
		if err != nil {
			panic(err)
		}

		err = pprof.StartCPUProfile(cpu)
		if err != nil {
			panic(err)
		}

		defer pprof.StopCPUProfile()

		mem, err := os.Create(filepath.Join(cmd.Profile, "mem.pprof"))
		if err != nil {
			panic(err)
		}

		defer func() {
			err := pprof.WriteHeapProfile(mem)
			if err != nil {
				panic(err)
			}
		}()
	}

	db, root := driver.MakeRoot()

	cwd, err := os.Getwd()
	if err != nil {
		panic(err)
	}

	readFile := func(path string) (*file.FileNode, error) {
		source, err := os.ReadFile(path)
		if err != nil {
			return nil, err
		}

		file, _ := syntax.Parse(db, path, string(source), file.ParseFile)
		return file, nil
	}

	type layer struct {
		name  string
		files []*file.FileNode
	}

	layers := make([]layer, 0, len(cmd.Lib)+1)
	for _, path := range cmd.Lib {
		path, err = filepath.Rel(cwd, path)
		if err != nil {
			return "", "", err
		}

		entries, err := os.ReadDir(path)
		if err != nil {
			return "", "", err
		}

		files := make([]*file.FileNode, 0, len(entries))
		for _, entry := range entries {
			if !entry.IsDir() && filepath.Ext(entry.Name()) == ".wipple" {
				file, err := readFile(filepath.Join(path, entry.Name()))
				if err != nil {
					return "", "", err
				}

				if file != nil {
					files = append(files, file)
				}
			}
		}

		layers = append(layers, layer{name: path, files: files})
	}

	var files layer
	files.files = make([]*file.FileNode, 0, len(cmd.Paths))
	for i, path := range cmd.Paths {
		path, err := filepath.Rel(cwd, path)
		if err != nil {
			return "", "", err
		}

		file, err := readFile(path)
		if err != nil {
			return "", "", err
		}

		if file != nil {
			if i > 0 {
				files.name += ", "
			}

			files.name += path

			files.files = append(files.files, file)
		}
	}

	layers = append(layers, files)

	var filters []database.FilterFunc
	for _, entry := range cmd.FilterLines {
		entry = strings.Trim(entry, " =")

		split := strings.SplitN(entry, ":", 2)
		if len(split) == 2 {
			path := split[0]
			line, err := strconv.Atoi(split[1])
			if err != nil {
				continue
			}

			filters = append(filters, database.LineFilter(path, line))
		} else {
			line, err := strconv.Atoi(split[0])
			if err != nil {
				continue
			}

			path := cmd.Paths[len(cmd.Paths)-1]
			path, err = filepath.Rel(cwd, path)
			if err != nil {
				return "", "", err
			}

			filters = append(filters, database.LineFilter(path, line))
		}
	}

	filter := func(node database.Node) bool {
		return len(filters) == 0 || slices.ContainsFunc(filters, func(f database.FilterFunc) bool {
			return f(node)
		})
	}

	for _, layer := range layers {
		_, err = fmt.Fprintf(os.Stderr, "Compiling %s...", layer.name)
		if err != nil {
			panic(err)
		}

		start := time.Now()
		driver.Compile(db, root, layer.files)
		duration := time.Since(start)

		_, err = fmt.Fprintf(os.Stderr, " done (%dms)\n", duration.Milliseconds())
		if err != nil {
			panic(err)
		}
	}

	var output strings.Builder

	if cmd.Facts {
		_, err := fmt.Fprintln(&output, colors.Title("Facts:"))
		if err != nil {
			panic(err)
		}

		db.Write(&output, filter)
	}

	feedbackCount := driver.WriteFeedback(db, filter, cmd.FilterFeedback, &output)
	if feedbackCount > 0 {
		return output.String(), "", fmt.Errorf("compilation failed with %d feedback item(s)", feedbackCount)
	}

	if run || cmd.Output != "" {
		outputPath := cmd.Output
		if outputPath == "" {
			outputPath = "index.js"
		}

		codegen := codegen.NewCodegen(db, outputPath, codegen.Options{
			Prelude:   nodePrelude + runtime,
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
			return output.String(), "", err
		}

		if cmd.Output != "" {
			err := os.WriteFile(outputPath, []byte(script), 0644)
			if err != nil {
				return output.String(), "", err
			}
		}

		if run {
			return output.String(), script, nil
		}
	}

	return output.String(), "", nil
}

func format(cmd *FormatCmd) error {
	source, err := os.ReadFile(cmd.Path)
	if err != nil {
		return err
	}

	formatted, syntaxError := syntax.Format(string(source))
	if syntaxError != nil {
		return fmt.Errorf("syntax error: %v", syntaxError)
	}

	fmt.Println(formatted)

	return nil
}
