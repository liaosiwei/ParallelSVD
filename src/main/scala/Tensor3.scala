/**
 * Created on 15/3/21.
 */
class Tensor3(x: Int, y: Int, z: Int) {
  val tensor = Array.ofDim[Double](z, y, x)
  val ranks = 3
  val dims = (x, y, z)

  lazy val norm = {
    var frob_norm = 0.0
    for {i <- 0 until x
      j <- 0 until y
      k <- 0 until z}
      frob_norm += math.pow(this.get(i, j, k), 2)
    math.sqrt(frob_norm)
  }

  def set(i: Int, j: Int, k: Int, value: Double): Unit = {
    tensor(k)(j)(i) = value
  }
  def get(i: Int, j: Int, k: Int): Double = tensor(k)(j)(i)

  def slice(from: Int, to: Int)  = None

  def sliceToMatrix(dim: Int, index: Int): Matrix = dim match {
    case 3 =>
      Matrix.from2DArray(tensor(index))
    case 2 => {
      val m = new Matrix(x, z)
      for {i <- 0 until x
           j <- 0 until z}
        m.set(i, j, tensor(j)(index)(i))
      m
    }
    case 1 => {
      val m = new Matrix(y, z)
      for {i <- 0 until y
           j <- 0 until z}
        m.set(i, j, tensor(j)(i)(index))
      m
    }
    case _ => Matrix(1, 1)
  }

  // this function only slice tensor along first order
  def slice_1st(start: Int, end: Int): Tensor3 = {
    val min_start = math.min(start, end)
    val max_end = math.max(start, end)
    val distance = max_end - min_start + 1
    val sliceTensor = Tensor3(distance, y, z)
    val (dim1, dim2, dim3) = sliceTensor.dims
    for {i <- 0 until dim1
         j <- 0 until dim2
         k <- 0 until dim3}
      sliceTensor.set(i, j, k, this.get(min_start + i, j, k))
    sliceTensor
  }

  def matricization(dim: Int): Matrix = dim match {
    case 1 => {
      val matrix = Matrix(x, y * z)
      for {k <- 0 until z
           j <- 0 until y
           i <- 0 until x}
        matrix.set(i, j + y*k, this.get(i, j, k))
      matrix
    }
    case 2 => {
      val matrix = Matrix(y, x * z)
      for {k <- 0 until z
           i <- 0 until x
           j <- 0 until y}
        matrix.set(j, i + x*k, this.get(i, j, k))
      matrix
    }
    case 3 => {
      val matrix = Matrix(z, x * y)
      for {j <- 0 until y
           i <- 0 until x
           k <- 0 until z}
        matrix.set(k, i + x*j, this.get(i, j, k))
      matrix
    }
    case _ => {
      println("matricization failed because of out of orders")
      Matrix(1, 1)
    }
  }

/*  def ttm(matrix: Matrix): Tensor3 = {

  }*/

  override def toString = {
    var str = ""
    for (i <- 0 until z)
      str += sliceToMatrix(3, i).toString() + "\n"
    str
  }
}

object Tensor3 {
  def apply(x: Int, y: Int, z: Int) = new Tensor3(x, y, z)

  def random(x: Int, y: Int, z: Int) = {
    val tensor = Tensor3(x, y, z)
    for {i <- 0 until x
         j <- 0 until y
         k <- 0 until z}
      tensor.set(i, j, k, util.Random.nextDouble())
    tensor
  }
}