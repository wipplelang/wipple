package typecheck

import (
	"wipple/database"
)

func NamedType[T Type](definition database.Node, name string, parameters []T) *ConstructedType {
	children := make([]Type, 0, len(parameters))
	for _, parameter := range parameters {
		children = append(children, parameter)
	}

	return &ConstructedType{
		Tag:      definition,
		Children: children,
		Display: func(children []func(root bool) string, root bool) string {
			s := name
			for _, child := range children {
				s += " " + child(false)
			}

			if root || len(children) == 0 {
				return s
			} else {
				return "(" + s + ")"
			}
		},
		Codegen: func(children []any, node func(node database.Node) string) any {
			return map[string]any{
				"type":       "named",
				"name":       node(definition),
				"parameters": children,
			}
		},
	}
}

func FunctionType[T Type](inputs []T, output T) *ConstructedType {
	children := make([]Type, 0, len(inputs)+1)
	children = append(children, output)
	for _, input := range inputs {
		children = append(children, input)
	}

	return &ConstructedType{
		Tag:      "function",
		Children: children,
		Display: func(children []func(root bool) string, root bool) string {
			output := children[0]
			inputs := children[1:]

			s := ""
			for i, input := range inputs {
				if i > 0 {
					s += " "
				}
				s += input(false)
			}

			s += " -> " + output(true)

			if root {
				return s
			}

			return "(" + s + ")"
		},
		Codegen: func(children []any, node func(node database.Node) string) any {
			output := children[0]
			inputs := children[1:]

			return map[string]any{
				"type":   "function",
				"inputs": inputs,
				"output": output,
			}
		},
	}
}

func TupleType[T Type](elements []T) *ConstructedType {
	children := make([]Type, 0, len(elements))
	for _, element := range elements {
		children = append(children, element)
	}

	return &ConstructedType{
		Tag:      "tuple",
		Children: children,
		Display: func(children []func(root bool) string, root bool) string {
			if len(children) == 0 {
				return "()"
			}

			if len(children) == 1 {
				return "(" + children[0](true) + ";)"
			}

			s := ""
			for i, child := range children {
				if i > 0 {
					s += "; "
				}

				s += child(true)
			}

			return "(" + s + ")"
		},
		Codegen: func(children []any, node func(node database.Node) string) any {
			return map[string]any{
				"type":     "tuple",
				"elements": children,
			}
		},
	}
}

func BlockType(output Type) *ConstructedType {
	return &ConstructedType{
		Tag:      "block",
		Children: []Type{output},
		Display: func(children []func(root bool) string, root bool) string {
			return "{" + children[0](true) + "}"
		},
		Codegen: func(children []any, node func(node database.Node) string) any {
			return map[string]any{
				"type":   "block",
				"output": children[0],
			}
		},
	}
}

func ParameterType(definition database.Node, name string) *ConstructedType {
	return &ConstructedType{
		Tag:         "parameter",
		Children:    []Type{definition},
		Instantiate: definition,
		Display: func(children []func(root bool) string, root bool) string {
			return name
		},
		Codegen: func(children []any, node func(node database.Node) string) any {
			return map[string]any{
				"type": "parameter",
				"name": node(definition),
			}
		},
	}
}
