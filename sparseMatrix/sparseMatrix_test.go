package sparseMatrix

import (
	"reflect"
	"strconv"
	"testing"
)

func TestNewSquareMatrix(t *testing.T) {
	type args struct {
		i int
	}
	tests := []struct {
		name string
		args args
		want *SquareMatrix
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := NewSquareMatrix(tt.args.i); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("NewSquareMatrix() = %v, want %v", got, tt.want)
			}
		})
	}
}

type Pair struct {
	x int
	y int
}

func TestSquareMatrix(t *testing.T) {
	// TODO: size of 2 breaks, why lmao
	for size := 3; size < 2000; size++ {
		t.Run(strconv.Itoa(size), func(t *testing.T) {
			sq := NewSquareMatrix(size)
			for i := 0; i < size; i++ {
				for j := 0; j < size; j++ {
					var want *Thing
					if got := sq.Idx(i, j); !reflect.DeepEqual(got, want) {
						t.Errorf("i,j=%d,%d expected nil: got %v want %v", i, j, got, want)
					}
					var p Thing
					p = Pair{x: i, y: j}
					sq.Insert(i, j, &p)
					if got, want := sq.Idx(i, j), &p; !reflect.DeepEqual(got, want) {
						t.Errorf("i,j=%d,%d just set: got %v want %v", i, j, got, want)
					}
				}
			}
			for i := 0; i < size; i++ {
				for j := 0; j < size; j++ {
					var p Thing
					p = Pair{x: i, y: j}
					if got, want := sq.Idx(i, j), &p; !reflect.DeepEqual(got, want) {
						t.Errorf("i,j=%d,%d read old: got %v want %v", i, j, got, want)
					}
				}
			}

			for _, tc := range []Pair{
				{x: 2, y: 2},
			} {
				var p Thing
				p = tc
				sq.Insert(tc.x, tc.y, &p)
				for i := 0; i < size; i++ {
					for j := 0; j < size; j++ {
						if i == tc.x && j == tc.y {
							continue
						}
						var str Thing
						str = "FUCKNOOO"
						sq.Insert(i, j, &str)
					}
				}

				if got, want := sq.Idx(tc.x, tc.y), p; *got != want {
					t.Errorf("i,j=%d,%d read old: got %v want %v", tc.x, tc.y, got, want)
				}
			}
		})
	}
}
