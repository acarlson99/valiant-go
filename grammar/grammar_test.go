package grammar

import (
	"fmt"
	"testing"
)

func TestParse(t *testing.T) {
	Parse()
}

func TestType(t *testing.T) {
	// s := "abbcd"
	// S -> aBSc
	// S -> abc
	// Ba -> aB
	// Bb -> bb
	g := Grammar{
		N:     nil,
		Sigma: nil,
		P:     nil,
		S:     "S",
	}
	fmt.Println(g)
}

// https://en.wikipedia.org/wiki/CYK_algorithm#Example
func TestSentence(t *testing.T) {
	// s := "she eats a fish with a fork"
	productionRules := ProductionRules{
		"S": {
			{"NP", "VP"},
		},
		"VP": {
			{"VP", "PP"},
			{"P", "NP"},
			{"eats"},
		},
		"PP": {
			{"P", "NP"},
		},
		"NP": {
			{"Det", "N"},
			{"she"},
		},
		"V": {
			{"eats"},
		},
		"P": {
			{"with"},
		},
		"N": {
			{"fish"},
			{"fork"},
		},
		"Det": {
			{"a"},
		},
	}
	g := MakeGrammar(
		[]Nonterminal{"S", "VP", "PP", "NP", "V", "P", "N", "Det"},
		[]Terminal{"eats", "she", "with", "fish", "fork", "a"},
		productionRules,
		"S",
	)
	fmt.Printf("%+v\n", g)
}
