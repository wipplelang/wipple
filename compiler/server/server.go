package server

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"

	"github.com/aws/aws-lambda-go/events"
	"github.com/aws/aws-lambda-go/lambda"
	"github.com/joho/godotenv"
	"github.com/rs/cors"
	"go.mongodb.org/mongo-driver/v2/mongo"
	"go.mongodb.org/mongo-driver/v2/mongo/options"
)

var Prelude string

var mongodb *mongo.Database

type Request struct {
	Compile       *CompileRequest       `json:"compile,omitempty"`
	Documentation *DocumentationRequest `json:"documentation,omitempty"`
	Format        *FormatRequest        `json:"format,omitempty"`
	GetShared     *GetSharedRequest     `json:"getShared,omitempty"`
	IdeInfo       *IdeInfoRequest       `json:"ideInfo,omitempty"`
	Share         *ShareRequest         `json:"share,omitempty"`
}

type InputMetadata struct {
	Library string `json:"library,omitempty"`
}

func Run(onLambda bool) error {
	godotenv.Load()

	mongoUri := os.Getenv("MONGODB_URI")
	mongoDbName := os.Getenv("MONGODB_DB_NAME")
	if mongoUri != "" && mongoDbName != "" {
		client, err := mongo.Connect(options.Client().ApplyURI(mongoUri))
		if err != nil {
			panic(err)
		}

		mongodb = client.Database(mongoDbName)
	}

	if onLambda {
		lambda.Start(func(request events.APIGatewayProxyRequest) (*events.APIGatewayProxyResponse, error) {
			var req Request
			err := json.Unmarshal([]byte(request.Body), &req)
			if err != nil {
				return &events.APIGatewayProxyResponse{
					StatusCode: 400,
					Body:       err.Error(),
				}, nil
			}

			res, status, err := handle(req)
			if err != nil {
				return &events.APIGatewayProxyResponse{
					StatusCode: status,
					Body:       err.Error(),
				}, nil
			}

			json, err := json.Marshal(res)
			if err != nil {
				return &events.APIGatewayProxyResponse{
					StatusCode: 500,
					Body:       err.Error(),
				}, nil
			}

			return &events.APIGatewayProxyResponse{
				StatusCode: status,
				Body:       string(json),
			}, nil
		})

		return nil
	} else {
		mux := http.NewServeMux()
		mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
			if r.Method != "POST" {
				http.Error(w, "expected POST request", 400)
				return
			}

			var req Request
			err := json.NewDecoder(r.Body).Decode(&req)
			if err != nil {
				http.Error(w, err.Error(), 400)
				return
			}

			res, status, err := handle(req)
			if err != nil {
				http.Error(w, err.Error(), status)
				return
			}

			w.Header().Set("Content-Type", "application/json")
			w.WriteHeader(status)
			json.NewEncoder(w).Encode(res)
		})

		port := os.Getenv("PORT")
		if port == "" {
			return fmt.Errorf("expected PORT")
		}

		fmt.Println("Listening on port", port)

		return http.ListenAndServe(":"+port, cors.AllowAll().Handler(mux))
	}
}

func handle(request Request) (any, int, error) {
	var response any
	var err error
	if request.Compile != nil {
		response, err = request.Compile.handle()
	} else if request.Documentation != nil {
		response, err = request.Documentation.handle()
	} else if request.Format != nil {
		response, err = request.Format.handle()
	} else if request.GetShared != nil {
		response, err = request.GetShared.handle()
	} else if request.IdeInfo != nil {
		response, err = request.IdeInfo.handle()
	} else if request.Share != nil {
		response, err = request.Share.handle()
	} else {
		return nil, 400, fmt.Errorf("invalid request type")
	}

	if err != nil {
		return nil, 500, err
	}

	return response, 200, nil
}
