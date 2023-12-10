package grammar_test

import (
	"fmt"
	"testing"

	"github.com/acarlson99/valiant-go/grammar"
)

func TestParse(t *testing.T) {
	grammar.Parse()
}

func TestType(t *testing.T) {
	s := "abbcd"
	// S -> aBSc
	// S -> abc
	// Ba -> aB
	// Bb -> bb
	g := grammar.Grammar{
		N:     nil,
		Sigma: nil,
		P:     nil,
		S:     grammar.Nonterminal{},
	}
	fmt.Println(g)
}
