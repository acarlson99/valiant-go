package sparseMatrix

import (
	"reflect"
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
	size := 50
	sq := NewSquareMatrix(size)
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			if got := sq.Idx(i, j); got != nil {
				// t.Errorf("i,j=%d,%d expected nil: got %v want %v", i, j, got, nil)
			}
			sq.Insert(i, j, Pair{x: i, y: j})
			if got, want := sq.Idx(i, j), Thing(Pair{x: i, y: j}); !reflect.DeepEqual(got, want) {
				t.Errorf("i,j=%d,%d just set: got %v want %v", i, j, got, want)
			}
		}
	}
	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			if got, want := sq.Idx(i, j), Thing(Pair{x: i, y: j}); !reflect.DeepEqual(got, want) {
				t.Errorf("i,j=%d,%d read old: got %v want %v", i, j, got, want)
			}
		}
	}
}
