package server

import (
	"wipple/syntax"
)

type FormatRequest struct {
	Code string `json:"code"`
}

type FormatResponse struct {
	Code *string `json:"code"`
}

func (request *FormatRequest) handle() (FormatResponse, error) {
	formatted, err := syntax.Format(request.Code)
	if err != nil || formatted == request.Code {
		return FormatResponse{Code: nil}, nil
	}

	return FormatResponse{Code: &formatted}, nil
}
