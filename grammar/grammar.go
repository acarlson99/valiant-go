package grammar

type Symbol string
type Nonterminal Symbol
type Terminal Symbol

type ProductionRule []Symbol

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

func (g *Grammar) Valiant(s string, n int) {

}
