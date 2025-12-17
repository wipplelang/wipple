package server

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"sync"
)

type Library struct {
	Metadata LibraryMetadata `json:"metadata"`
	Files    []File          `json:"files"`
}

type LibraryMetadata struct {
	Library string          `json:"library,omitempty"`
	Ide     json.RawMessage `json:"ide,omitempty"`
}

var (
	librariesMutex = sync.RWMutex{}
	libraries      = map[string]*Library{}
)

func fetchLibrary(name string) (*Library, bool, error) {
	librariesMutex.RLock()
	cached, ok := libraries[name]
	librariesMutex.RUnlock()
	if ok {
		return cached, true, nil
	}

	libraryUrl := os.Getenv("LIBRARY_URL")
	if libraryUrl == "" {
		return nil, false, fmt.Errorf("missing LIBRARY_URL environment variable")
	}

	url := fmt.Sprintf("%s/%s.json", libraryUrl, name)

	response, err := http.Get(url)
	if err != nil {
		return nil, false, err
	}

	defer response.Body.Close()

	if response.StatusCode != http.StatusOK {
		return nil, false, fmt.Errorf("failed to fetch library: %s", response.Status)
	}

	var library Library
	err = json.NewDecoder(response.Body).Decode(&library)
	if err != nil {
		return nil, false, err
	}

	librariesMutex.Lock()
	libraries[name] = &library
	librariesMutex.Unlock()

	return &library, false, nil
}
