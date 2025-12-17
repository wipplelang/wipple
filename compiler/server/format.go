package server

import (
	"wipple/syntax"
)

type FormatRequest struct {
	Code string `json:"code"`
}

type FormatResponse struct {
	Code string `json:"code"`
}

func (request *FormatRequest) handle() (FormatResponse, error) {
	formatted, err := syntax.Format(request.Code)
	if err != nil {
		// Return the original code if it can't be parsed
		return FormatResponse{Code: request.Code}, nil
	}

	return FormatResponse{Code: formatted}, nil
}
