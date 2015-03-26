/**
 * Created on 15/3/21.
 */
class Matrix(val row: Int, val col: Int) {
  val matrix = Array.ofDim[Double](col, row)
  val size = (row, col)
  lazy val norm = {
    var n = 0.0d
    for (i <- 0 until row; j <- 0 until col)
      n += math.pow(get(i, j), 2)
    math.sqrt(n)
  }

  def isEmpty = row == 0 && col == 0
  def get(i: Int, j: Int) = matrix(j)(i)

  def set(i: Int, j: Int, value: Double): Unit = {
    matrix(j)(i) = value
  }
  def set(colIndex: Int, arr: Vec) =
    0 until arr.size map (x => matrix(colIndex)(x) = arr.get(x))

  def getCol(colIndex: Int): Vec = Vec.fromArray(this.matrix(colIndex))

  def getRow(rowIndex: Int): Vec = {
    var list: List[Double] = Nil
    for (i <- 0 until col)
      list = this.matrix(i)(rowIndex)::list
    Vec.fromList(list.reverse)
  }

  def addByCol(other: Matrix): Matrix = {
    val newMatrix = Matrix(row, col + other.col)
    for (i <- 0 until row; j <- 0 until newMatrix.col) {
      if (j < col)
        newMatrix.set(i, j, get(i, j))
      else
        newMatrix.set(i, j, other.get(i, j - col))
    }
    newMatrix
  }

  def sliceByCol(from: Int, to: Int): Matrix = {
    val max_col = math.max(from, to)
    val min_col = math.min(from, to)
    val slice_col = max_col - min_col + 1
    val sliceMatrix = new Matrix(row, slice_col)
    for {i <- 0 until row
         j <- 0 until slice_col} {
      sliceMatrix.set(i, j, this.get(i, j + min_col))
    }
    sliceMatrix
  }

  def sliceByColEqually(pieces: Int): List[Matrix] = {
    val colPerPiece = col / pieces
    var mod = col % pieces
    var res: List[Matrix] = Nil
    var index = 0
    for (i <- 0 until pieces) {
      val tempCol = if (mod > 0) colPerPiece + 1 else colPerPiece
      res = sliceByCol(index, index + tempCol - 1) :: res
      index += tempCol
      mod -= 1
    }
    res.reverse
  }

  def t: Matrix = {
    val transposedMatrix = new Matrix(col, row)
    for {i <- 0 until col
         j <- 0 until row} {
      transposedMatrix.set(i, j, this.get(j, i))
    }
    transposedMatrix
  }

  def dot(that: Matrix) = {
    val (r, c) = that.size
    if (this.col != r) {
      println("matrix dot operation dimension is not matched")
      Matrix(1, 1)
    } else {
      val dotMatrix = new Matrix(this.row, c)
      for (i <- 0 until dotMatrix.row;
           j <- 0 until dotMatrix.col) {
        val rowVec = getRow(i)
        val colVec = that.getCol(j)
        dotMatrix.set(i, j, rowVec dot colVec)
      }
      dotMatrix
    }
  }

  def normalizeU = {
    val vecArray = (0 until col map (c => {
      val vec = getCol(c)
      (vec / vec.norm, vec.norm)})).sortWith((x, y) => x._2 > y._2)
    val matrixU = Matrix(row, col)
    for (i <- 0 until col; j <- 0 until row) {
      matrixU.set(j, i, vecArray(i)._1.get(j))
    }
    matrixU
  }

  override def toString(): String = {
    val (r, c) = this.size
    var str = "[\n"
    for (i <- 0 until r) {
      for (j <- 0 until c) {
        str += " " + this.get(i, j)
      }
      str += "\n"
    }
    str += "]"
    str
  }
}

object Matrix {
  def apply(row: Int, col: Int) = new Matrix(row, col)
  def random(row: Int, col: Int) = {
    val m = Matrix(row, col)
    for {i <- 0 until row
         j <- 0 until col}
      m.set(i, j, util.Random.nextDouble())
    m
  }
  def from2DArray(arr: Array[Array[Double]]): Matrix = {
    val col = arr.size
    val row = arr(0).size
    val m = Matrix(row, col)
    for {i <- 0 until row
         j <- 0 until col}
      m.set(i, j, arr(j)(i))
    m
  }
  def emptyMatrix() = new Matrix(0, 0)
}