package server

import "encoding/json"

type IdeInfoRequest struct {
	InputMetadata
}

type IdeInfoResponse struct {
	Info []json.RawMessage `json:"info"`
}

func (request *IdeInfoRequest) handle() (IdeInfoResponse, error) {
	var info []json.RawMessage

	libraryName := request.Library
	for libraryName != "" {
		library, _, err := fetchLibrary(libraryName)
		if err != nil {
			return IdeInfoResponse{}, err
		}

		if library.Metadata.Ide != nil {
			info = append(info, library.Metadata.Ide)
		}

		libraryName = library.Metadata.Library
	}

	return IdeInfoResponse{Info: info}, nil
}
