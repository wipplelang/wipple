package server

import (
	"context"
	"fmt"

	"go.mongodb.org/mongo-driver/v2/bson"
)

type GetSharedRequest struct {
	Id string `json:"id"`
}

type GetSharedResponse struct {
	Runtime string `json:"runtime"`
	Code    string `json:"code"`
}

func (request *GetSharedRequest) handle() (GetSharedResponse, error) {
	ctx := context.TODO()

	if mongodb == nil {
		return GetSharedResponse{}, fmt.Errorf("sharing not enabled")
	}

	collection := mongodb.Collection("playgrounds")

	var response GetSharedResponse
	err := collection.FindOne(ctx, bson.M{"_id": request.Id}).Decode(&response)
	if err != nil {
		return GetSharedResponse{}, fmt.Errorf("playground not found")
	}

	return response, nil
}
