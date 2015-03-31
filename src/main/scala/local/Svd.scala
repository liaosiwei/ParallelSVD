package local

/**
 * Created on 15/3/31.
 */
import util.control.Breaks._
import datatype.Matrix

class Svd(row: Int, col: Int) {
  val matrix = Matrix.random(row, col)

  def orthogonalize() = {
    val block = matrix
    val tol = math.pow(block.norm, 2) * 1e-15
    val maxStep = 75
    var sweepCount = 0
    breakable {
      for (step <- 0 until maxStep) {
        sweepCount += 1
        var converged = true

        println(s"sweep count is $sweepCount")
        for {i <- 0 until (block.col - 1)
             j <- (i + 1) until block.col} {

          val di = block.getCol(i)
          val dj = block.getCol(j)
          val dii = di dot di
          val djj = dj dot dj
          val dij = di dot dj

          if (math.abs(dij) > tol)
            converged = false
          if (dij != 0) {
            val tao = (djj - dii) / (2 * dij)
            val t = math.signum(tao) / (math.abs(tao) + math.sqrt(math.pow(tao, 2) + 1))
            val c = 1.0 / math.sqrt(math.pow(t, 2) + 1)
            val s = t * c
            //update data block
            for (k <- 0 until block.row) {
              val res1 = block.get(k, i) * c - block.get(k, j) * s
              val res2 = block.get(k, i) * s + block.get(k, j) * c
              block.set(k, i, res1)
              block.set(k, j, res2)
            }
          }
        }
        if (converged)
          break()
      }
    }
    println(s"total sweep count is $sweepCount")
    block
  }
}

object Svd{
  def main (args: Array[String]) {
    if (args.length != 2)
      println("wrong parameter list")
    else {
      val row = args(0).toInt
      val col = args(1).toInt
      val startTime = System.currentTimeMillis()
      val svd = new Svd(row, col)
      svd.orthogonalize()
      val endTime = System.currentTimeMillis()
      println("time spend is " + (endTime - startTime) /  1000.0 + "s")
    }
  }
}