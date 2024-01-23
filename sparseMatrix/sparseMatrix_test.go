package sparseMatrix

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
)

type Pair struct {
	x int
	y int
}

func (p *Pair) Add(r Ring[int]) Ring[int] {
	if at, bt := reflect.TypeOf(p), reflect.TypeOf(r); at != bt {
		return nil
	}
	p2 := r.(*Pair)
	return &Pair{p.x + p2.x, p.y + p2.y}
}

func (p *Pair) Multiply(r Ring[int]) Ring[int] {
	if at, bt := reflect.TypeOf(p), reflect.TypeOf(r); at != bt {
		return nil
	}
	p2 := r.(*Pair)
	return &Pair{p.x * p2.x, p.y * p2.y}
}

// TODO: invalid writes should not write (-1,-1 for instance)
func populateCanary(m Matrix[string]) {
	for i := 0; i < m.Width(); i++ {
		for j := 0; j < m.Width(); j++ {
			var str string
			str = fmt.Sprintf("CANARY %d,%d", i, j)
			m.Insert(i, j, str)
		}
	}
}

func makeNoOverwriteTest(size int) func(t *testing.T) {
	return func(t *testing.T) {
		t.Parallel()
		sq := NewSquareMatrix[*Pair](size)
		for i := 0; i < size; i++ {
			for j := 0; j < size; j++ {
				if got, want := sq.Index(i, j), (*Pair)(nil); !reflect.DeepEqual(got, want) {
					t.Errorf("i,j=%d,%d expected nil: got %v want %v", i, j, got, want)
				}
				p := &Pair{x: i, y: j}
				sq.Insert(i, j, p)
				if got, want := sq.Index(i, j), p; !reflect.DeepEqual(got, want) {
					t.Errorf("i,j=%d,%d just set: got %v want %v", i, j, got, want)
				}
			}
		}
		for i := 0; i < size; i++ {
			for j := 0; j < size; j++ {
				p := &Pair{x: i, y: j}
				if got, want := sq.Index(i, j), p; !reflect.DeepEqual(got, want) {
					t.Errorf("i,j=%d,%d read old: got %v want %v", i, j, got, want)
				}
			}
		}
	}
}

func makeEdgeTest(size int) func(t *testing.T) {
	return func(t *testing.T) {
		t.Parallel()
		sq := NewSquareMatrix[Pair](size)
		// populateCanary(sq)
		testCases := []Pair{
			{x: 0, y: 0},
			{x: size - 1, y: size - 1},
			{x: 0, y: size - 1},
			{x: size - 1, y: 0},
			{x: size / 2, y: size / 3},
			{x: size / 3, y: size / 2},
		}
		for _, tc := range testCases {
			sq.Insert(tc.x, tc.y, tc)
		}
		for _, tc := range testCases {
			if got, want := sq.Index(tc.x, tc.y), tc; got != want {
				t.Errorf("i,j=%d,%d read old: got %v want %v", tc.x, tc.y, got, want)
			}
		}
	}
}

func makeOOBTest(size int) func(t *testing.T) {
	return func(t *testing.T) {
		t.Parallel()
		sq := NewSquareMatrix[string](size)
		for _, tc := range []struct {
			x int
			y int
			s string
		}{{-1, -1, "hello :)"},
			{size, size, "wowww"},
			{size, 0, "hehe"}} {
			sq.Insert(tc.x, tc.y, tc.s)
			if !IsEmpty(sq.UL) || !IsEmpty(sq.UR) || !IsEmpty(sq.BL) || !IsEmpty(sq.BR) {
				t.Fatalf("Invalid inputs %v should have written to no cells", tc)
			}
		}
	}
}

func TestIterativeSquareMatrix(t *testing.T) {
	for size := 2; size < 20; size++ {
		size := size
		t.Run(fmt.Sprintf("iter-size-%04d", size), func(t *testing.T) {
			t.Parallel()
			t.Run("no-overwrite", makeNoOverwriteTest(size))

			t.Run("edges", makeEdgeTest(size))

			// only on N^2 bc of how size is calculated
			if size&(size-1) == 0 {
				t.Run("out of bounds", makeOOBTest(size))
			}
		})
	}
	for size := 1020; size < 1040; size++ {
		size := size
		t.Run(fmt.Sprintf("iter-size-%04d", size), func(t *testing.T) {
			t.Parallel()
			t.Run("no-overwrite", makeNoOverwriteTest(size))

			t.Run("edges", makeEdgeTest(size))

			// only on N^2 bc of how size is calculated
			if size&(size-1) == 0 {
				t.Run("out of bounds", makeOOBTest(size))
			}
		})
	}
}

// func TestAddMatrix(t *testing.T) {
// 	// NewSquareMatrix[]()
// }

func TestConstructMatrix(t *testing.T) {
	tests := []struct {
		name string
		arg  string
		want Matrix[rune]
	}{
		{
			arg:  "abcdef",
			name: "test len=6",
			want: &SquareMatrix[rune]{
				w: 8,
				UL: &SquareMatrix[rune]{
					w: 4,
					UL: &SquareMatrix[rune]{
						w:  2,
						UL: &UnitMatrix[rune]{v: 'a'},
						UR: &EmptyMatrix[rune]{w: 1},
						BL: &EmptyMatrix[rune]{w: 1},
						BR: &UnitMatrix[rune]{v: 'b'},
					}, // ab
					UR: &EmptyMatrix[rune]{w: 2},
					BL: &EmptyMatrix[rune]{w: 2},
					BR: &SquareMatrix[rune]{
						w:  2,
						UL: &UnitMatrix[rune]{v: 'c'},
						UR: &EmptyMatrix[rune]{w: 1},
						BL: &EmptyMatrix[rune]{w: 1},
						BR: &UnitMatrix[rune]{v: 'd'},
					}, // cd
				}, // abcd
				UR: &EmptyMatrix[rune]{w: 4},
				BL: &EmptyMatrix[rune]{w: 4},
				BR: &SquareMatrix[rune]{
					w: 4,
					UL: &SquareMatrix[rune]{
						w:  2,
						UL: &UnitMatrix[rune]{v: 'e'},
						UR: &EmptyMatrix[rune]{w: 1},
						BL: &EmptyMatrix[rune]{w: 1},
						BR: &UnitMatrix[rune]{v: 'f'},
					}, // ef
					UR: &EmptyMatrix[rune]{w: 2},
					BL: &EmptyMatrix[rune]{w: 2},
					BR: &EmptyMatrix[rune]{w: 2},
				},
			},
		},
		{
			arg:  "ab",
			name: "test len=2",
			want: &SquareMatrix[rune]{
				w:  2,
				UL: &UnitMatrix[rune]{v: 'a'},
				UR: &EmptyMatrix[rune]{w: 1},
				BL: &EmptyMatrix[rune]{w: 1},
				BR: &UnitMatrix[rune]{v: 'b'},
			},
		},
		{
			arg:  "a",
			name: "singleton",
			want: &UnitMatrix[rune]{v: 'a'},
		},
		{
			arg:  "",
			name: "empty str",
			want: &UnitMatrix[rune]{},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := ConstructMatrix(tt.arg); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("ConstructMatrix() = %v, want %v", got, tt.want)
			}
			s := ConstructMatrix(tt.arg).String()
			for _, s := range strings.Split(s, "\n") {
				fmt.Println("|", s, "|")
			}
			// fmt.Println(s)
		})
	}
}
