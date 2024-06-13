# Parsing with Valiant's Algorithm

This project is for parsing using an algorithm similar to CYK (https://en.wikipedia.org/wiki/CYK_algorithm#Valiant's_algorithm) which uses matrix multiplication to combine grammar tokens and produce a parse tree

### Usage

```sh
cabal run valiant-hs --enable-profiling -- --gram=demo/bnf.bnf demo/num.bnf
```

Alternatively you could use this as a library e.g.

```hs
import Grammar.Chomsky
import Grammar.Convert
import Grammar.Tokenize
import Valiant

input = "<P> ::= <P><P>\n"
gram = convert bnfCFG -- or some other grammar of your choosing
(Just tokF) = matchTokenizer "chars"
valiantParse gram $ tokF input
```

### Files

```sh
src/
  Grammar/      # Grammar defs and utils for converting Context Free to Chomsky Normal Form
    Chomsky     # CNF rammar
    ContextFree # ContextFree grammar and utils for converting to CNF
    Convert     # CFG -> CNF
    Tokenize    # Some very simple tokenizers
  Matrix/
    Sparse      # Sparse matrix which satisfies `Ring` interface.  It is dependantly typed and encodes its size in its type
    N           # A wrapping type for `SparseMatrix` which "forgets" the size to make interfacing with it easier
  Nat           # Natural numbers which can be used at the type level
  Ring          # Defines a ring (really a semi-ring) structure
  RingParse     # Creates a parser which satisfies the Ring interface
  Valiant       # The meat of the valiant's algorithm is the function `valiantParse` which takes a list of strings and a grammar, converts these to ValiantMatrix form, and returns a list of parse trees
  Vec           # Dependantly typed vector
  VecN          # Wrapper for Vec which forgets its length
```

### Limitations

* Due to the 2-dimensional nature of the matrix we must convert CFG grammars to CNF before parsing-- this means that the produced parse tree will not be the same as the initial grammar.  This CFG->CNF conversion can be reversed by tracking changes made to production rules and reversing them after parsing (but I haven't added that)
* I encode all RingParse objects in (ProductionRules,RingParse) tuples-- this is not memory efficient, but I couldn't find a better way to do it
* Currently the input string must be aligned on 2^n boundaries because the matrix is square and it returns the upper-rightmost cell.  This will be fixed

### Futher Optimizations

Note that the matrix I use is "maximally sparse", meaning that it is most efficient when it is mostly empty i.e. when there are not many valid parses of the input string.  This is usually the case for programming languages, but some grammars do not lend themselves well to this (like lisp (because it has so many nested lists (well because it is list processing (obviously)))).  Most optimizations are noted with this in mind.

* Grammars can be expanded before parsing to reduce number of tokens

```bnf
<L> ::= "(" <L2> ")"
<L2> ::= <NUM> "," <L2>
```

This recursive rule adds ambiguity when parsing because it will produce many `<L2>` tokens.  Better would be to "blow up" the grammar like so:

```bnf
<L> ::= "(" <L2> ")"
<L2> ::= <L2_1> | <L2_2> | <L2_3> | <L2_4> | <L2_N>
<L2_1> ::= <NUM>
<L2_2> ::= <NUM> "," <NUM>
<L2_3> ::= <NUM> "," <NUM> "," <NUM>
<L2_4> ::= <NUM> "," <NUM> "," <NUM> "," <NUM>
<L2_N> ::= <NUM> "," <NUM> "," <NUM> "," <NUM> "," <L2>
```

This reduces ambiguity by consuming up to four tokens at a time in the `<L2>` rule instead of consuming an unknown number and producing more tokens than necessary.

* Create grammar reversal so parsing returns a parse tree matching the input grammar
