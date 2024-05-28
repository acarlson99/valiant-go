package grammar

type CNFRules []Rule

type Rule interface {
	Produce() []Symbol
}

type Nullary struct {
	Rule
}

func (n Nullary) Produce() []Symbol { return nil }

type Unary struct {
	t Symbol
}

func (r Unary) Produce() []Symbol {
	return []Symbol{r.t}
}

type Binary struct {
	a Symbol
	b Symbol
}

func (r Binary) Produce() []Symbol {
	return []Symbol{r.a, r.b}
}
