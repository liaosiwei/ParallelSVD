import datatype.Matrix

val m = Matrix(3, 4)
m.set(1, 2, 1.2f)
m.set(2, 2, 2f)
m.set(1, 3, 3f)
val mm = m
val norm = mm.norm
m.t
m.sliceByCol(1, 1)
val randm = Matrix.random(4, 6)
val vec1 = randm.getCol(2)
val vec2 = randm.getCol(3)
val vec3 = randm.getRow(0)
val col = randm.getCol(1)
col / 2
val v1 = m.sliceByCol(1, 2)
v1.normalizeU
val slices = randm.sliceByColEqually(5)
val other = Matrix.random(3, 2)
v1.addByCol(other)
v1.t
val arr = Array.ofDim[Double](2, 2)
arr(0)(0) = 1.2f
arr(1)(0) = 3.0f
arr(0)(1) = 0.4f
arr(1)(1) = 6f
Matrix.from2DArray(arr)
Matrix.emptyMatrix()
val m1 = Matrix(2, 2)
m1.set(0, 0, 1)
m1.set(0, 1, 2)
m1.set(1, 0, 1)
m1.set(1, 1, 1)
m1
m1.normalizeU

