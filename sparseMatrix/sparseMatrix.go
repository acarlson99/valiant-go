package sparseMatrix

type Thing interface{}

type Matrix interface {
	Idx(i, j int) Thing
	Insert(i, j int, v Thing) Matrix
	Width() int
	Height() int
}

// partition = N/2 where N is the width in cells of this section of the matrix

type SquareMatrix struct {
	UL *Matrix
	UR *Matrix
	BL *Matrix
	BR *Matrix

	w int
}

func NewSquareMatrix(n int) *SquareMatrix {
	// i = int(math.Pow(math.Ceil(math.Sqrt(float64(i))), 2))
	i := 1 << 0
	for i < n {
		i = i << 1
	}
	return &SquareMatrix{w: i}
}

func (m *SquareMatrix) Width() int {
	if m == nil {
		return 0
	}
	return m.w
}

func (m *SquareMatrix) Height() int {
	if m == nil {
		return 0
	}
	return m.w
}

func (m *SquareMatrix) quarter(i, j int) *Matrix {
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

func (m *SquareMatrix) setQuarter(i, j int, mx *Matrix) *Matrix {
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

// n=2 w=2 => 0
func safeHalve(n int, m *SquareMatrix) int {
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

func (m *SquareMatrix) Idx(i, j int) Thing {
	if m == nil {
		return nil
	}
	q := m.quarter(i, j)
	if q == nil {
		return q
	}
	return (*q).Idx(safeHalve(i, m), safeHalve(j, m)) // TODO: this divide by 2 does not work
}

func (m *SquareMatrix) Insert(i, j int, v Thing) Matrix {
	q := m.quarter(i, j)
	if q == nil {
		// TODO: decide what matrix to insert, should be square or unary
		var mx Matrix
		if m.w == 2 {
			mx = &UnitMatrix{}
		} else {
			mx = &SquareMatrix{w: m.w / 2}
		}
		mx = mx.Insert(safeHalve(i, m), safeHalve(j, m), v)
		m.setQuarter(i, j, &mx)
	} else {
		(*q).Insert(safeHalve(i, m), safeHalve(j, m), v)
	}
	return m
}

type UnitMatrix struct {
	v Thing
}

func (m *UnitMatrix) Width() int { return 1 }

func (m *UnitMatrix) Height() int { return 1 }

func (m *UnitMatrix) Idx(i, j int) Thing {
	return m.v
}

func (m *UnitMatrix) Insert(i, j int, v Thing) Matrix {
	if m == nil {
		return &UnitMatrix{v: v}
	}
	m.v = v
	return m
}

type UpperRightTriangularMatrix struct {
	UR *SquareMatrix
	UL *UpperRightTriangularMatrix
	BR *UpperRightTriangularMatrix

	w, h int
}
