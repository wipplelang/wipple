package server

import (
	"context"
	"fmt"
	"sync/atomic"
	"time"

	gonanoid "github.com/matoous/go-nanoid/v2"
	"go.mongodb.org/mongo-driver/v2/bson"
	"go.mongodb.org/mongo-driver/v2/mongo"
	"go.mongodb.org/mongo-driver/v2/mongo/options"
)

type ShareRequest struct {
	Runtime string `json:"runtime"`
	Code    string `json:"code"`
}

type ShareResponse struct {
	Id string `json:"id"`
}

const expiration int32 = 7 * 86400 // 7 days

var shareInit atomic.Bool

func (request *ShareRequest) handle() (ShareResponse, error) {
	ctx := context.TODO()

	if mongodb == nil {
		return ShareResponse{}, fmt.Errorf("sharing not enabled")
	}

	collection := mongodb.Collection("playgrounds")

	// Expire documents
	if !shareInit.Swap(true) {
		_, err := collection.Indexes().CreateOne(ctx, mongo.IndexModel{
			Keys:    bson.D{{Key: "createdAt", Value: 1}},
			Options: options.Index().SetExpireAfterSeconds(expiration),
		})

		if err != nil {
			return ShareResponse{}, err
		}
	}

	id, err := gonanoid.New()
	if err != nil {
		return ShareResponse{}, err
	}

	_, err = collection.InsertOne(ctx, bson.M{
		"_id":       id,
		"createdAt": time.Now(),
		"runtime":   request.Runtime,
		"code":      request.Code,
	})

	if err != nil {
		return ShareResponse{}, err
	}

	return ShareResponse{Id: id}, nil
}
