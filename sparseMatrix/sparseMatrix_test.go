package sparseMatrix

import (
	"fmt"
	"reflect"
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
			if sq.UL != nil || sq.UR != nil || sq.BL != nil || sq.BR != nil {
				t.Fatalf("Invalid inputs %v should have written to no cells", tc)
			}
		}
	}
}

func TestSquareMatrix(t *testing.T) {
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

func TestAddMatrix(t *testing.T) {
	// NewSquareMatrix[]()
}
