val tensor = Tensor3.random(2, 2,2)
tensor.get(0, 0, 0)
tensor.get(0, 1, 0)
tensor.get(1, 0, 1)
//tensor.sliceToMatrix(2, 1)
//tensor.sliceToMatrix(1, 1)
// test tensor matricization
tensor.set(0, 0, 0, 1) ;tensor.set(1, 0, 0, 3) ;tensor.set(0, 1, 0, 2)
tensor.set(1, 1, 0, 4) ;tensor.set(0, 0, 1, 5) ;tensor.set(1, 0, 1, 7)
tensor.set(0, 1, 1, 6) ;tensor.set(1, 1, 1, 8)
tensor
val m1 = tensor.matricization(1)
val m2 = tensor.matricization(2)
val m3 = tensor.matricization(3)

// test slice tensor along the 1st order
val slice_tnsr1 = tensor.slice_1st(0, 0)
val slice_tnsr2 = tensor.slice_1st(1, 1)
val m4 = slice_tnsr1.matricization(3)
val m5 = slice_tnsr2.matricization(3)
val m6 = tensor.matricization(3)

val m7 = slice_tnsr1.matricization(1)
val m8 = slice_tnsr2.matricization(1)
val m9 = tensor.matricization(1)

val m10 = slice_tnsr1.matricization(2)
val m11 = slice_tnsr2.matricization(2)
val m12 = tensor.matricization(2)
// test tensor norm
val frob_norm = tensor.norm