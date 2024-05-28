package grammar

type Symbol interface {
	Name() string
}

type Nonterminal string

func (s Nonterminal) Name() string {
	return string(s)
}

type Terminal string

func (s Terminal) Name() string {
	return string(s)
}

type ProductionRule struct {
	name Nonterminal
	rhs  []Symbol
}

type ProductionRules map[Nonterminal][]ProductionRule

func (ProductionRule) Produce([]Symbol, Nonterminal, []Symbol) []Symbol {
	return nil
}

type Grammar struct {
	// G = (N, sigma, P, S)
	N     map[Symbol]Nonterminal
	Sigma map[Symbol]Terminal
	P     ProductionRules
	S     Nonterminal
}

func MakeGrammar(nts []Nonterminal, ts []Terminal, ps ProductionRules, s Nonterminal) Grammar {
	ntm := make(map[Symbol]Nonterminal)
	for _, sym := range nts {
		ntm[Symbol(sym)] = sym
	}
	tm := make(map[Symbol]Terminal)
	for _, sym := range ts {
		tm[Symbol(sym)] = sym
	}
	return Grammar{
		N:     ntm,
		Sigma: tm,
		P:     ps,
		S:     s,
	}
}
