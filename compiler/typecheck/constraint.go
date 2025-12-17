package typecheck

import (
	"reflect"

	"wipple/database"
)

type instantiateFunc = func(solver *Solver, source database.Node, replacements map[database.Node]database.Node, substitutions *map[database.Node]Type) Constraint

type runFunc = func(solver *Solver) bool

type Constraint struct {
	key               reflect.Value
	IsActive          bool
	ShouldInstantiate bool
	Node              database.Node
	Instance          *Instance
	debug             func() string
	instantiate       instantiateFunc
	run               runFunc
}

type constraintConfig struct {
	Node        database.Node
	Instance    *Instance
	Debug       func() string
	Instantiate instantiateFunc
	Run         runFunc
}

func newConstraint(key any, config constraintConfig) Constraint {
	return Constraint{
		key:               reflect.ValueOf(key),
		IsActive:          true,
		ShouldInstantiate: true,
		Node:              config.Node,
		Instance:          config.Instance,
		debug:             config.Debug,
		instantiate:       config.Instantiate,
		run:               config.Run,
	}
}

func (c Constraint) String() string {
	return c.debug()
}
