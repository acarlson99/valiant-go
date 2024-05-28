package main

import (
	"fmt"
)

func cykParse(grammar map[string][]string, input string) bool {
	n := len(input)
	table := make([][]map[string]bool, n)

	for i := range table {
		table[i] = make([]map[string]bool, n)
		for j := range table[i] {
			table[i][j] = make(map[string]bool)
		}
	}

	// Initialization: filling in the table based on the terminal rules
	for i := 0; i < n; i++ {
		for _, production := range grammar[input[i:i+1]] {
			table[i][i][production] = true
		}
	}

	// Filling in the table based on non-terminal rules
	for length := 2; length <= n; length++ {
		for i := 0; i <= n-length; i++ {
			j := i + length - 1
			for k := i; k < j; k++ {
				for left := range table[i][k] {
					for right := range table[k+1][j] {
						for _, production := range grammar[left+right] {
							table[i][j][production] = true
						}
					}
				}
			}
		}
	}

	// Check if the start symbol is in the top-right corner of the table
	startSymbol := "S" // Replace with your actual start symbol
	return table[0][n-1][startSymbol]
}

func main() {
	// Example grammar in Chomsky normal form
	grammar := map[string][]string{
		"S": {"AB", "BC"},
		"A": {"BA", "a"},
		"B": {"CC", "b"},
		"C": {"AB", "a"},
	}

	// Example input string
	input := "baaba"

	result := cykParse(grammar, input)

	if result {
		fmt.Printf("The input string \"%s\" is accepted by the grammar.\n", input)
	} else {
		fmt.Printf("The input string \"%s\" is not accepted by the grammar.\n", input)
	}
}
