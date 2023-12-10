package grammar

import (
	"fmt"
)

type Nonterminal struct {
}

type Terminal struct {
}

type ProductionRule struct {
}

func (ProductionRule) Produce([]Symbol, Nonterminal, []Symbol) []Symbol {
	return nil
}

// type Symbol string

type Symbol struct {
	nt Nonterminal
	t  Terminal
}

type Grammar struct {
	// G = (N, sigma, P, S)
	N     map[Symbol][]Nonterminal
	Sigma map[Symbol][]Terminal
	P     map[Symbol][]ProductionRule
	S     Nonterminal
}

func Parse() {
	nt := Nonterminal{"a"}
	fmt.Println(nt)
}

func b(nt Nonterminal) {}
