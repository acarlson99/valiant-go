package sparseMatrix

type Matrix[T any] interface {
	Index(i, j int) T
	Insert(i, j int, v T) Matrix[T]
	Width() int
	Height() int
}

// partition = N/2 where N is the width in cells of this section of the matrix

type SquareMatrix[T any] struct {
	UL *Matrix[T]
	UR *Matrix[T]
	BL *Matrix[T]
	BR *Matrix[T]

	w int // TODO: rebrand to `size` or actually track width+height, not this shit wtf
}

func NewSquareMatrix[T any](n int) *SquareMatrix[T] {
	i := 1 << 0 // TODO: remove hack
	for i < n {
		i = i << 1
	}
	return &SquareMatrix[T]{w: i}
}

func (m *SquareMatrix[T]) Width() int {
	if m == nil {
		return 0
	}
	return m.w
}

func (m *SquareMatrix[T]) Height() int {
	if m == nil {
		return 0
	}
	return m.w
}

func (m *SquareMatrix[T]) quarter(i, j int) *Matrix[T] {
	subI := m.w / 2
	subJ := m.w / 2
	var left bool
	var top bool
	if i < subI {
		left = true
	}
	if j < subJ {
		top = true
	}
	if left && top {
		return m.UL
	} else if left {
		return m.BL
	} else if top {
		return m.UR
	} else {
		return m.BR
	}
}

func (m *SquareMatrix[T]) setQuarter(i, j int, mx *Matrix[T]) *Matrix[T] {
	subI := m.w / 2
	subJ := m.w / 2
	var left bool
	var top bool
	if i < subI {
		left = true
	}
	if j < subJ {
		top = true
	}
	if left && top {
		m.UL = mx
		return m.UL
	} else if left {
		m.BL = mx
		return m.BL
	} else if top {
		m.UR = mx
		return m.UR
	} else {
		m.BR = mx
		return m.BR
	}
}

func partitionIndex[T any](n int, m *SquareMatrix[T]) int {
	half := m.w / 2
	if n < half {
		return n
	}
	return n - half
}

// X,Y = 5,6
// | X X X X | X X X X |
// | X X X X | X X X X |
// | X X X X | X X X X |
// | X X X X | X X X X |
// +---------+---------+
// | X X X X | X X X X |
// | X X X X | X X X X |
// | X X X X | X O X X |
// | X X X X | X X X X |
// BR X,Y = 1,2
// | X X | X X |
// | X X | X X |
// +-----+-----+
// | X O | X X |
// | X X | X X |
// BL X,Y = 0,1
// X O
// X X
// UR X,Y = 0,0

func (m *SquareMatrix[T]) inBounds(i, j int) bool {
	return i >= 0 && j >= 0 && i < m.w && j < m.w
}

func (m *SquareMatrix[T]) Index(i, j int) T {
	if m == nil {
		return *new(T)
	}
	q := m.quarter(i, j)
	if q == nil {
		return *new(T)
	}
	return (*q).Index(partitionIndex(i, m), partitionIndex(j, m))
}

func (m *SquareMatrix[T]) Insert(i, j int, v T) Matrix[T] {
	if !m.inBounds(i, j) {
		return nil
	}
	q := m.quarter(i, j)
	if q == nil {
		// TODO: decide what matrix to insert, should be square or unary
		var mx Matrix[T]
		if m.w == 2 {
			mx = &UnitMatrix[T]{}
		} else {
			mx = &SquareMatrix[T]{w: m.w / 2}
		}
		mx = mx.Insert(partitionIndex(i, m), partitionIndex(j, m), v)
		m.setQuarter(i, j, &mx)
		return m
	}
	(*q).Insert(partitionIndex(i, m), partitionIndex(j, m), v)
	return m
}

type UnitMatrix[T any] struct {
	v T
}

func (m *UnitMatrix[T]) Width() int { return 1 }

func (m *UnitMatrix[T]) Height() int { return 1 }

func (m *UnitMatrix[T]) Index(i, j int) T {
	return m.v
}

func (m *UnitMatrix[T]) Insert(i, j int, v T) Matrix[T] {
	if m == nil {
		return &UnitMatrix[T]{v: v}
	}
	m.v = v
	return m
}

type UpperRightTriangularMatrix[T any] struct {
	UR *SquareMatrix[T]
	UL *UpperRightTriangularMatrix[T]
	BR *UpperRightTriangularMatrix[T]

	w, h int
}
