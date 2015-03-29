package remote

/**
 * Created on 15/3/29.
 */

import akka.actor._
import datatype.Matrix

class WorkerActor(id: Int) extends Actor{
  var upDataBlock = Matrix.emptyMatrix()
  var dnDataBlock = Matrix.emptyMatrix()
  var converged: Boolean = true
  var tol = 0.0
  var workerNumber = 0
  var iteration = 0
  var master: ActorRef = _
  var workerPathList: List[String] = Nil

  def receive = {
    case RetrieveBlocks => {
      master ! new ReturnedBlock(upDataBlock, dnDataBlock)
    }
    case InnerOrth => {
      //println(s"Worker ${id}: in InnerOrth")
      converged = true
      //println(s"worker ${id} innerorth received")
      upDataBlock = innerOrth(upDataBlock)
      dnDataBlock = innerOrth(dnDataBlock)
      //println(s"after innerorth: ${upDataBlock} ${dnDataBlock}")
      master ! InnerOrthDone
    }

    case OutterOrth => {
      //println(s"Worker ${id}: in OutterOrth")
      val res = outterOrth(upDataBlock, dnDataBlock)
      upDataBlock = res._1
      dnDataBlock = res._2
      master ! OutterOrthDone
      //println(s"in worker ${id} OutterOrth received and get result ${res._1} ${res._2}")
    }

    case RoundRobinProcess => {
      //println(s"Worker ${id}: in RoundRobinProcess")
      //println(s"in worker ${id} begin roundrobin process")
      val worker2 = context.actorSelection(workerPathList(1))
      worker2 ! new RoundRobin(dnDataBlock, "dn")
    }

    case RoundRobin(dataBlock, flag) => {
      //println(s"Worker ${id}: in RoundRobin")
      flag match {
        case "up" => {
          if (id == 1) {
            dnDataBlock = upDataBlock
            upDataBlock = dataBlock
            master ! RoundRobinProcessDone
          }
          else {
            val worker = context.actorSelection(workerPathList(id-2))
            worker ! new RoundRobin(upDataBlock, "up")
            upDataBlock = dataBlock
          }
        }
        case "dn" => {
          if (id == workerNumber) {
            val worker = context.actorSelection(workerPathList(id-2))
            worker ! new RoundRobin(dnDataBlock, "up")
            dnDataBlock = dataBlock
          }
          else {
            val worker = context.actorSelection(workerPathList(id))
            worker ! new RoundRobin(dnDataBlock, "dn")
            dnDataBlock = dataBlock
          }
        }
      }
    }
    case QueryConverged => {
      //println(s"Worker ${id}: in QueryConverged")
      master ! new IterationEnd(converged)
    }

    case InitialData(up, dn, value, pathList) => {
      master = sender
      upDataBlock = up
      dnDataBlock = dn
      tol = value
      workerNumber = pathList.size
      workerPathList = pathList
      //println(s"worker ${id} received data ${upDataBlock} and ${dnDataBlock}")
    }
  }

  def innerOrth(block: Matrix): Matrix = {
    val matrix = block
    for {i <- 0 until (block.col - 1)
         j <- (i + 1) until block.col} {

      val di = block.getCol(i)
      val dj = block.getCol(j)
      val dii = di dot di
      val djj = dj dot dj
      val dij = di dot dj

      if (math.abs(dij) > tol)
        this.converged = false
      if (dij != 0) {
        val tao = (djj - dii) / (2 * dij)
        val t = math.signum(tao) / (math.abs(tao) + math.sqrt(math.pow(tao, 2) + 1))
        val c = 1.0 / math.sqrt(math.pow(t, 2) + 1)
        val s = t * c
        //update data block
        for (k <- 0 until block.row) {
          val res1 = block.get(k, i) * c - block.get(k, j) * s
          val res2 = block.get(k, i) * s + block.get(k, j) * c
          matrix.set(k, i, res1)
          matrix.set(k, j, res2)
        }
      }
    }
    matrix
  }

  def outterOrth(block1: Matrix, block2: Matrix): (Matrix, Matrix) = {
    val matrix1 = block1
    val matrix2 = block2

    for (i <- 0 until block1.col; j <- 0 until block2.col) {
      val di = block1.getCol(i)
      val dj = block2.getCol(j)
      val dii = di dot di
      val djj = dj dot dj
      val dij = di dot dj

      if (math.abs(dij) > tol)
        this.converged = false
      if (dij != 0) {
        val tao = (djj - dii) / (2 * dij)
        val t = math.signum(tao) / (math.abs(tao) + math.sqrt(math.pow(tao, 2) + 1))
        val c = 1.0 / math.sqrt(math.pow(t, 2) + 1)
        val s = t * c

        //update data block
        for (k <- 0 until block1.row) {
          val res1 = block1.get(k, i) * c - block2.get(k, j) * s
          val res2 = block1.get(k, i) * s + block2.get(k, j) * c
          matrix1.set(k, i, res1)
          matrix2.set(k, j, res2)
        }
      }
    }
    (matrix1, matrix2)
  }
}
