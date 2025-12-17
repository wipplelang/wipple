package syntax

import (
	"fmt"
	"testing"

	"wipple/database"
)

type Error struct {
	Message   string
	Reason    string
	Committed string
	Span      database.Span
}

func (e *Error) String() string {
	s := ""

	if testing.Testing() {
		s = fmt.Sprintf("%v: %s", e.Span, e.Message)
	} else {
		s = e.Message
	}

	if e.Committed != "" {
		s += fmt.Sprintf(" %s", e.Committed)
	}

	return s
}

type SyntaxErrorFact Error

func (fact SyntaxErrorFact) String() string {
	s := fact.Message

	if fact.Committed != "" {
		s += " " + fact.Committed
	}

	return s
}

func GetSyntaxErrorFact(node database.Node) (SyntaxErrorFact, bool) {
	return database.GetFact[SyntaxErrorFact](node)
}

func SetSyntaxErrorFact(node database.Node, err Error) {
	database.SetFact(node, SyntaxErrorFact(err))
}
