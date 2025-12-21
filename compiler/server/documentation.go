package server

import (
	"fmt"
	"wipple/database"
	"wipple/feedback"
	"wipple/queries"
)

type DocumentationRequest struct {
	InputMetadata
	Name string `json:"name"`
}

type DocumentationResponse struct {
	Documentation *Documentation `json:"documentation,omitempty"`
}

type Documentation struct {
	Declaration string `json:"declaration"`
	Docs        string `json:"docs"`
}

func (request *DocumentationRequest) handle() (DocumentationResponse, error) {
	libraryName := request.InputMetadata.Library
	if libraryName == "" {
		return DocumentationResponse{}, fmt.Errorf("missing library")
	}

	db, _, err := compileLibrary(libraryName)
	if err != nil {
		return DocumentationResponse{}, err
	}

	filter := func(node database.Node) bool {
		return true
	}

	data, ok := database.ContainsNode(db, func(node database.Node) (queries.DocumentationData, bool) {
		var data queries.DocumentationData
		ok := false
		queries.Documentation(db, node, filter, func(name string, d queries.DocumentationData) {
			if name == request.Name {
				ok = true
				data = d
			}
		})

		return data, ok
	})

	if !ok {
		return DocumentationResponse{}, nil
	}

	render := feedback.NewRender(db)
	render.WriteComments(data.Comments)
	docs := render.Finish()

	return DocumentationResponse{
		Documentation: &Documentation{
			Declaration: data.Declaration,
			Docs:        docs,
		},
	}, nil
}
