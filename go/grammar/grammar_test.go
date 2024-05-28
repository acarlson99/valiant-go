package grammar

import (
	"fmt"
	"testing"
)

func TestParse(t *testing.T) {
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
			{name: "S", rhs: []Symbol{Nonterminal("NP"), Nonterminal("VP")}},
		},
		"VP": {
			{name: "VP", rhs: []Symbol{Nonterminal("VP"), Nonterminal("PP")}},
			{name: "VP", rhs: []Symbol{Nonterminal("P"), Nonterminal("NP")}},
			{name: "VP", rhs: []Symbol{Terminal("eats")}},
		},
		"PP": {
			{name: "PP", rhs: []Symbol{Nonterminal("P"), Nonterminal("NP")}},
		},
		"NP": {
			{name: "NP", rhs: []Symbol{Nonterminal("Det"), Nonterminal("N")}},
			{name: "NP", rhs: []Symbol{Terminal("she")}},
		},
		"V": {
			{name: "V", rhs: []Symbol{Terminal("eats")}},
		},
		"P": {
			{name: "P", rhs: []Symbol{Terminal("with")}},
		},
		"N": {
			{name: "N", rhs: []Symbol{Terminal("fish")}},
			{name: "N", rhs: []Symbol{Terminal("fork")}},
		},
		"Det": {
			{name: "Det", rhs: []Symbol{Terminal("a")}},
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
