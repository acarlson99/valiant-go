package sparseMatrix

import (
	"fmt"
	"reflect"
	"strings"
)

type Matrix[T any] interface {
	Ring[T]

	Index(i, j int) T
	Insert(i, j int, v T) Matrix[T]
	Width() int
	Height() int

	String() string
}

type SquareMatrix[T any] struct {
	UL Matrix[T]
	UR Matrix[T]
	BL Matrix[T]
	BR Matrix[T]

	w int
}

// TODO: addition: set union
//       multiplication: set(a) -> set(b) -> set(C(a,b))

// function V to compute for one level up
// solve what to do for singleton matrix
//   The singleton matrix effectively "lifts" its value

// partition = N/2 where N is the width in cells of this section of the matrix

type Ring[T any] interface {
	Add(b Ring[T]) Ring[T]
	Multiply(b Ring[T]) Ring[T]
	// Compare(b Ring[T]) bool
}

func f() {
	var r Ring[int]
	r = new(RingSet[int])
	fmt.Println(r)
}

type RingInt struct {
	Ring[int]
	v int
}

func (a *RingInt) Add(r Ring[int]) Ring[int] {
	if at, bt := reflect.TypeOf(a), reflect.TypeOf(r); at != bt {
		return nil
	}
	return &RingInt{v: a.v + r.(*RingInt).v}
}

func (a *RingInt) Multiply(r Ring[int]) Ring[int] {
	if at, bt := reflect.TypeOf(a), reflect.TypeOf(r); at != bt {
		return nil
	}
	return &RingInt{v: a.v * r.(*RingInt).v}
}

func (a *RingInt) Compare(r *RingInt) bool {
	return a.v == r.v
}

type RingSet[T comparable] struct {
	Ring[T]
	s map[T]bool
}

func (a *RingSet[T]) Add(r Ring[T]) Ring[T] {
	if at, bt := reflect.TypeOf(a), reflect.TypeOf(r); at != bt {
		return nil
	}
	b := r.(*RingSet[T])
	c := new(RingSet[T])
	for k := range a.s {
		if _, ok := b.s[k]; ok {
			c.s[k] = true
		}
	}
	return c
}

func (a *RingSet[T]) Multiply(r Ring[T]) Ring[T] {
	if at, bt := reflect.TypeOf(a), reflect.TypeOf(r); at != bt {
		return nil
	}
	b := r.(*RingSet[T])
	c := new(RingSet[T])
	for k := range a.s {
		c.s[k] = true
	}
	for k := range b.s {
		c.s[k] = true
	}
	return c
}

func (m *SquareMatrix[T]) Add(b Ring[T]) Ring[T] {
	return nil
}

func (m *SquareMatrix[T]) Multiply(b Ring[T]) Ring[T] {
	return nil
}

func nextSquare(n int) int {
	i := 1 << 0 // TODO: remove hack
	for i < n {
		i = i << 1
	}
	return i
}

func (m *SquareMatrix[T]) String() string {
	if m == nil {
		return ""
	}
	UL := m.UL
	if UL == nil {
		UL = &EmptyMatrix[T]{w: m.w >> 1}
	}
	a := strings.Split((UL).String(), "\n")
	UR := m.UR
	if UR == nil {
		UR = &EmptyMatrix[T]{w: m.w >> 1}
	}
	b := strings.Split((UR).String(), "\n")
	BL := m.BL
	if BL == nil {
		BL = &EmptyMatrix[T]{w: m.w >> 1}
	}
	c := strings.Split((BL).String(), "\n")
	BR := m.BR
	if BR == nil {
		BR = &EmptyMatrix[T]{w: m.w >> 1}
	}
	d := strings.Split((BR).String(), "\n")

	for i, v := range b {
		a[i] = strings.Join([]string{a[i], v}, "")
	}
	for i, v := range d {
		c[i] = strings.Join([]string{c[i], v}, "")
	}

	return strings.Join([]string{strings.Join(a, "\n"), strings.Join(c, "\n")}, "\n")
}

func NewSquareMatrix[T any](n int) *SquareMatrix[T] {
	i := nextSquare(n)
	return &SquareMatrix[T]{UL: &EmptyMatrix[T]{w: i >> 1}, UR: &EmptyMatrix[T]{w: i >> 1}, BL: &EmptyMatrix[T]{w: i >> 1}, BR: &EmptyMatrix[T]{w: i >> 1}, w: i}
	// return &SquareMatrix[T]{w: i}
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

func convertSquareMatrix[T any](a Ring[T]) Matrix[T] {
	if a == nil {
		return nil
	}
	return a.(Matrix[T])
}

func (m *SquareMatrix[T]) quarter(i, j int) Matrix[T] {
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

func (m *SquareMatrix[T]) setQuarter(i, j int, mx Matrix[T]) Matrix[T] {
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
	return q.Index(partitionIndex(i, m), partitionIndex(j, m))
}

func IsEmpty[T any](m Matrix[T]) bool {
	switch m.(type) {
	case *EmptyMatrix[T]:
		return true
	default:
		return false
	}
}

func (m *SquareMatrix[T]) Insert(i, j int, v T) Matrix[T] {
	if !m.inBounds(i, j) {
		return nil
	}
	q := m.quarter(i, j)
	if q == nil || IsEmpty[T](q) {
		if m.w <= 2 {
			q = &UnitMatrix[T]{}
		} else {
			q = NewSquareMatrix[T](m.w >> 1)
		}
	}
	m.setQuarter(i, j, q.Insert(partitionIndex(i, m), partitionIndex(j, m), v))
	return m
}

type UnitMatrix[T any] struct {
	v T
}

func (m *UnitMatrix[T]) Add(b Ring[T]) Ring[T] {
	return m
}

func (m *UnitMatrix[T]) Multiply(b Ring[T]) Ring[T] {
	return m
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

func GenericPrintf(value interface{}) string {
	s := ""
	switch v := value.(type) {
	case int:
		s = fmt.Sprintf("%d", v)
	case float64:
		s = fmt.Sprintf("%f", v)
	case string:
		s = v
	case rune:
		s = fmt.Sprintf("%c", v)
	default:
		s = fmt.Sprintf("%v", value)
	}
	l := len(s)
	dv := l / maxLen
	md := l % maxLen
	a := strings.Repeat(" ", dv)
	b := strings.Repeat(" ", md)
	return strings.Join([]string{a, s, b}, "")
}

func (m *UnitMatrix[T]) String() string {
	if m == nil {
		return " "
	}
	s := GenericPrintf(m.v)
	// s := fmt.Sprintf("%v", m.v)
	return s
}

type EmptyMatrix[T any] struct {
	w int
}

func (m *EmptyMatrix[T]) Width() int                     { return m.w }
func (m *EmptyMatrix[T]) Height() int                    { return m.w }
func (m *EmptyMatrix[T]) Index(i, j int) T               { return *new(T) }
func (m *EmptyMatrix[T]) Insert(i, j int, v T) Matrix[T] { return m }
func (m *EmptyMatrix[T]) Add(a Ring[T]) Ring[T]          { return m }
func (m *EmptyMatrix[T]) Multiply(a Ring[T]) Ring[T]     { return m }

var maxLen = 1

func (m *EmptyMatrix[T]) String() string {
	ss := []string{}
	s := strings.Repeat(" ", (maxLen+1)*m.w)
	for i := 0; i < m.w; i++ {
		ss = append(ss, s)
	}
	return strings.Join(ss, "\n")
}

type UpperRightTriangularMatrix[T any] struct {
	sq *SquareMatrix[T]
}

func NewUpperRightTriangularMatrix[T any](size int) *UpperRightTriangularMatrix[T] {
	return &UpperRightTriangularMatrix[T]{sq: NewSquareMatrix[T](size)}
}

func (t *UpperRightTriangularMatrix[T]) Index(i, j int) T {
	t.sq.BL = nil
	return t.sq.Index(i, j)
}

func (t *UpperRightTriangularMatrix[T]) Insert(i, j int, v T) Matrix[T] {
	t.sq.BL = nil
	return t.sq.Insert(i, j, v)
}

func (t *UpperRightTriangularMatrix[T]) Width() int {
	t.sq.BL = nil
	return t.sq.Width()
}

func (t *UpperRightTriangularMatrix[T]) Height() int {
	t.sq.BL = nil
	return t.sq.Height()
}

func (t *UpperRightTriangularMatrix[T]) Add(b Ring[T]) Ring[T] {
	t.sq.BL = nil
	return t.sq.Add(b)
}

func (t *UpperRightTriangularMatrix[T]) Multiply(b Ring[T]) Ring[T] {
	t.sq.BL = nil
	return t.sq.Multiply(b)
}

// func V[T any](a *UpperRightTriangularMatrix[T], x *SquareMatrix[T], b *UpperRightTriangularMatrix[T]) *SquareMatrix[T] {
// 	a11, a12, a22 := a.sq.UL.(*UpperRightTriangularMatrix[T]), a.sq.UR.(*SquareMatrix[T]), a.sq.BR.(*UpperRightTriangularMatrix[T])
// 	x11, x12, x21, x22 := x.UL.(*SquareMatrix[T]), x.UR.(*SquareMatrix[T]), x.BL.(*SquareMatrix[T]), x.BR.(*SquareMatrix[T])
// 	b11, b12, b22 := b.sq.UL.(*UpperRightTriangularMatrix[T]), b.sq.UR.(*SquareMatrix[T]), b.sq.BR.(*UpperRightTriangularMatrix[T])
// 	y21 := V(a11, x21, b11)
// 	y11 := V(a11, x11.Add(a12).Multiply(y21).(*SquareMatrix[T]), b11)
// 	y22 := V(a22, x22.Add(y21).Multiply(b12).(*SquareMatrix[T]), b22)
// 	y12 := V(a11, x12.Add(a12).Multiply(y22.Add(y11).Multiply(b12)).(*SquareMatrix[T]), b22)
// 	return &SquareMatrix[T]{UL: y11, UR: y12, BL: y21, BR: y22}
// }

func ConstructMatrix(s string) Matrix[rune] {
	l := len(s)
	var mat Matrix[rune]
	size := nextSquare(l)
	if size == 1 {
		mat = &UnitMatrix[rune]{}
	} else {
		mat = NewSquareMatrix[rune](size)
	}
	for i, c := range s {
		mat.Insert(i, i, c)
	}
	return mat
}
