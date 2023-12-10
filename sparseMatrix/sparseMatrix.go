package sparseMatrix

type Matrix interface {
	Idx(i, j int) interface{}
	Insert(i, j int, v interface{})
	Size() (int, int) // i,j width,height
}

type SquareMatrix struct {
	UL Matrix
	UR Matrix
	BL Matrix
	BR Matrix
}

func (m *SquareMatrix) Size() (int, int) {
	ulw, ulh := m.UL.Size()
	urw, _ := m.UR.Size()
	_, blh := m.BL.Size()
	return ulw + urw, ulh + blh
}

func (m *SquareMatrix) Idx(i, j int) interface{} {
	w, h := m.Size()
	var left bool
	var top bool
	subI := w / 2
	subJ := h / 2
	if i < subI {
		left = true
	}
	if j < subJ {
		top = true
	}
	if left && top {
		return m.UL.Idx(subI, subJ)
	} else if left {
		return m.BL.Idx(subI, subJ)
	} else if top {
		return m.UR.Idx(subI, subJ)
	} else {
		return m.BR.Idx(subI, subJ)
	}
}

type UpperRightTriangularMatrix struct {
}
