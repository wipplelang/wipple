package codegen

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"slices"
	"strings"

	"wipple/database"
	"wipple/typecheck"
	"wipple/vendored/sourcemap"
	"wipple/visit"
)

type CodegenFormat int

const (
	ModuleFormat CodegenFormat = iota
	IifeFormat
)

type Options struct {
	Prelude   string
	Format    CodegenFormat
	Input     string
	Sourcemap bool
}

type mapping struct {
	span   database.Span
	index  sourcemap.SourceIndex
	line   int
	column int
}

type Codegen struct {
	Db         *database.Db
	OutputPath string
	Options    Options

	output        strings.Builder
	mappings      map[database.Span]*mapping
	sourceIndices map[string]sourcemap.SourceIndex
	line          int
	column        int

	nodes        []database.Node
	writtenTypes map[string]writtenType
}

type writtenType struct {
	index      int
	typeString string
}

// Lines are 1-based and columns are 0-based
var startLine = 1
var startColumn = 0

func NewCodegen(db *database.Db, outputPath string, options Options) *Codegen {
	return &Codegen{
		Db:         db,
		OutputPath: outputPath,
		Options:    options,

		output:        strings.Builder{},
		mappings:      map[database.Span]*mapping{},
		sourceIndices: map[string]sourcemap.SourceIndex{},
		line:          startLine,
		column:        startColumn,

		nodes:        []database.Node{},
		writtenTypes: map[string]writtenType{},
	}
}

func (c *Codegen) Node(node database.Node) string {
	index := slices.Index(c.nodes, node)
	if index == -1 {
		c.nodes = append(c.nodes, node)
		index = len(c.nodes) - 1
	}

	return fmt.Sprintf("_%d", index)
}

func (c *Codegen) WriteNode(span database.Span, node database.Node) {
	c.WriteString(span, c.Node(node))
}

func (c *Codegen) WriteLine() {
	c.output.WriteByte('\n')
	c.line++
	c.column = 0
}

func (c *Codegen) WriteString(span database.Span, s string) {
	c.writeMapping(span)
	n, err := c.output.WriteString(s)
	if err != nil {
		panic(err)
	}

	c.column += n
}

func (c *Codegen) appendString(s string) {
	_, err := c.output.WriteString(s)
	if err != nil {
		panic(err)
	}
}

func (c *Codegen) WriteType(span database.Span, ty typecheck.Type) error {
	typeCode, err := c.ty(ty, true)
	if err != nil {
		return err
	}

	writtenType := typeCode.(writtenType)
	c.WriteString(span, fmt.Sprintf("/**! %s */ typeCache[%d]", writtenType.typeString, writtenType.index))

	return nil
}

type Write interface {
	database.Node
	Codegen(c *Codegen) error
}

func (c *Codegen) Write(node database.Node) error {
	if write, ok := node.(Write); ok {
		return write.Codegen(c)
	} else {
		panic(fmt.Sprintf("node is missing `Write` method: %s", database.DisplayNode(node)))
	}
}

func (c *Codegen) Error(node database.Node) error {
	return fmt.Errorf("cannot codegen %s", database.DisplayNode(node))
}

func (c *Codegen) ty(ty typecheck.Type, root bool) (any, error) {
	// Get the latest type
	if node, ok := ty.(database.Node); ok {
		if fact, ok := database.GetFact[typecheck.TypedFact](node); ok && fact.Group != nil {
			ty = fact.Group.Types[0]
		}
	}

	var constructed *typecheck.ConstructedType
	var ok bool
	if constructed, ok = ty.(*typecheck.ConstructedType); !ok {
		return nil, fmt.Errorf("unresolved type: %v", database.DisplayNode(ty.(database.Node)))
	}

	children := make([]any, 0, len(constructed.Children))
	if constructed.Instantiate == nil {
		for _, child := range constructed.Children {
			childCode, err := c.ty(child, false)
			if err != nil {
				return nil, err
			}

			children = append(children, childCode)
		}
	}

	typeJson := constructed.Codegen(children, c.Node)

	if root {
		typeCode, err := json.Marshal(typeJson)
		if err != nil {
			return nil, err
		}

		typeCodeString := string(typeCode)

		if writtenType, ok := c.writtenTypes[typeCodeString]; ok {
			return writtenType, nil
		}

		index := len(c.writtenTypes)
		writtenType := writtenType{
			index:      index,
			typeString: typeCodeString,
		}
		c.writtenTypes[typeCodeString] = writtenType

		return writtenType, nil
	} else {
		return typeJson, nil
	}
}

func (c *Codegen) writeDefinitions() error {
	var err error
	database.ContainsFact(c.Db, func(node database.Node, fact visit.DefinedFact) (struct{}, bool) {
		var body database.Node
		switch definition := fact.Definition.(type) {
		case *visit.TraitDefinition:
			err = c.writeInstances(definition.Node)
			if err != nil {
				return struct{}{}, true
			}
		case *visit.ConstantDefinition:
			if definition.Assigned {
				body = definition.Value
			}
		case *visit.InstanceDefinition:
			body = definition.Value
		}

		if body == nil {
			return struct{}{}, false
		}

		span := database.GetSpanFact(node)
		c.WriteString(span, fmt.Sprintf("/**! %s */ ", database.DisplayNode(node)))
		c.WriteString(span, "async function ")
		c.WriteNode(span, node)
		c.WriteString(span, "(types) {")
		c.WriteLine()
		c.WriteString(span, "return ")
		c.Write(body)
		c.WriteString(span, ";")
		c.WriteLine()
		c.WriteString(span, "}")
		c.WriteLine()

		return struct{}{}, false
	})

	return err
}

func (c *Codegen) writeInstances(trait database.Node) error {
	span := database.GetSpanFact(trait)
	instances, _ := database.GetFact[typecheck.InstancesFact](trait)

	c.WriteString(span, "const ")
	c.WriteNode(span, trait)
	c.WriteString(span, " = [")
	c.WriteLine()

	for _, instance := range instances {
		if instance.Error {
			continue // skip error instances that have no value
		}

		span := database.GetSpanFact(instance.Node)
		c.WriteString(span, "[")
		c.WriteNode(span, instance.Node)
		c.WriteString(span, ", {")
		for parameter, substitution := range *instance.Substitutions {
			c.WriteNode(span, parameter)
			c.WriteString(span, ": ")

			err := c.WriteType(span, substitution)
			if err != nil {
				return err
			}

			c.WriteString(span, ", ")
		}
		c.WriteString(span, "}],")
		c.WriteLine()
	}

	c.WriteString(span, "];")
	c.WriteLine()

	return nil
}

func (c *Codegen) String(root database.Node, files []database.Node) (string, error) {
	generator := sourcemap.NewGenerator(c.OutputPath)
	for _, node := range files {
		span := database.GetSpanFact(node)
		index := generator.AddSource(span.Path)
		err := generator.SetSourceContent(index, span.Source)
		if err != nil {
			return "", err
		}
		c.sourceIndices[span.Path] = index
	}

	rootSpan := database.GetSpanFact(root)

	switch c.Options.Format {
	case ModuleFormat:
		c.WriteString(rootSpan, "export default async function(runtime) {")
	case IifeFormat:
		c.WriteString(rootSpan, "(async (runtime) => {")
	}
	c.WriteLine()

	err := c.writeDefinitions()
	if err != nil {
		return "", err
	}

	c.WriteString(rootSpan, "const types = {};")
	c.WriteLine()

	for _, file := range files {
		err := c.Write(file)
		if err != nil {
			return "", err
		}
	}

	switch c.Options.Format {
	case ModuleFormat:
		c.appendString("};\n")
	case IifeFormat:
		c.appendString(fmt.Sprintf("})(%s);\n", c.Options.Input))
	}

	typeCodes := make([]string, len(c.writtenTypes))
	for _, writtenType := range c.writtenTypes {
		typeCodes[writtenType.index] = writtenType.typeString
	}

	var types strings.Builder
	for _, typeCode := range typeCodes {
		types.WriteString(typeCode)
		types.WriteString(",\n")
	}

	typeCache := fmt.Sprintf("const typeCache = [\n%s,\n];\n", types.String())

	prelude := c.Options.Prelude + typeCache
	preludeLines := strings.Count(prelude, "\n")

	for _, mapping := range c.mappings {
		mapping.line += preludeLines
	}

	mappings := make([]*mapping, 0, len(c.mappings))
	for _, mapping := range c.mappings {
		mappings = append(mappings, mapping)
	}
	slices.SortFunc(mappings, func(left *mapping, right *mapping) int {
		if left.line != right.line {
			return left.line - right.line
		}

		if left.column != right.column {
			return left.column - right.column
		}

		return database.CompareSpans(left.span, right.span)
	})

	for _, mapping := range mappings {
		err = generator.AddSourceMapping(mapping.line, mapping.column, mapping.index, mapping.span.Start.Line, mapping.span.Start.Column-1)
		if err != nil {
			return "", err
		}
	}

	if c.Options.Sourcemap {
		var sourcemapComment strings.Builder
		sourcemapComment.WriteString("\n//# sourceMappingURL=data:application/json;base64,")

		encoder := base64.NewEncoder(base64.StdEncoding, &sourcemapComment)
		_, err = encoder.Write([]byte(generator.String()))
		if err != nil {
			return "", err
		}
		err = encoder.Close()
		if err != nil {
			return "", err
		}

		c.output.WriteString(sourcemapComment.String())
		c.output.WriteString("\n")
	}

	return prelude + c.output.String(), nil
}

func (c *Codegen) writeMapping(span database.Span) {
	if _, exists := c.mappings[span]; !exists {
		c.mappings[span] = &mapping{
			span:   span,
			index:  c.sourceIndices[span.Path],
			line:   c.line,
			column: c.column,
		}
	}
}
