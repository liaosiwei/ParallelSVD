/**
 * Created on 15/3/25.
 */
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._

case class Initialization(workNumber: Int, maxStep: Int, row: Int, col: Int)
case object InnerOrth
case object InnerOrthDone
case object OutterOrth
case object OutterOrthDone
case object RoundRobinProcess
case object RoundRobinProcessDone
case class RoundRobin(block: Matrix, flag: String)
case object IsConverged
case class IterationEnd(converged: Boolean)
case object QueryConverged
case object RetrieveBlocks
case class ReturnedBlock(up: Matrix, dn: Matrix)
case object HasDone
case class InitialData(upBlock: Matrix, dnBlock: Matrix, tol: Double, workerNumber: Int)

class SvdMaster extends Actor{
  var workNumber = 0
  var matrix = Matrix.emptyMatrix()
  var tol = 0d
  var firstProcessNumber = 0
  var outterOrthNumber = 0
  var secondProcessNumber = 0
  var iteration = 0
  var allConverged = true
  var actorList: List[ActorRef] = Nil
  var mainThread: ActorRef = _
  var lastProcessNumber = 0
  var matrixU = Matrix.emptyMatrix()
  var sweepCount = 0
  var maxSteps = 75

  def receive = {
    case Initialization(w, s, r, c) => {
      //println("in master initialization")
      //split a matrix by columns with equal size
      workNumber = w
      maxSteps = s
      matrix = Matrix.random(r, c)
      tol = math.pow(matrix.norm, 2) * 1e-15

      println(s"initialized matrix size is ${matrix.row} x ${matrix.col}")

      val blockList = matrix.sliceByColEqually(workNumber)
      for (i <- 1 to workNumber) {
        val actor = context.actorOf(Props(new SvdWorker(i)), name = "worker" + i)
        val block = blockList(i-1).sliceByColEqually(2)
        actor ! new InitialData(block(0), block(1), tol, workNumber)
        actorList = actor :: actorList
      }

      for (actor <- actorList)
        actor ! InnerOrth
    }
    case InnerOrthDone => {
      firstProcessNumber += 1
      if (firstProcessNumber == workNumber) {
        firstProcessNumber = 0
        actorList.map(a => a ! OutterOrth)
        //println("Master: in InnerOrthDone")
      }
    }

    case OutterOrthDone => {
      outterOrthNumber += 1
      if (outterOrthNumber == workNumber) {
        outterOrthNumber = 0
        val worker = actorList.last
        worker ! RoundRobinProcess
        //println("Master: in OutterOrthDone")
      }
    }

    case IterationEnd(c) => {
      secondProcessNumber += 1
      allConverged = allConverged && c
      if (secondProcessNumber == workNumber) {
        secondProcessNumber = 0
        if (allConverged) {
          actorList.map(actor => actor ! RetrieveBlocks)
        }
        else {
          actorList.map(a => a ! InnerOrth)
          allConverged = true
        }
        //println("Master: in IterationEnd")
      }
    }

    case ReturnedBlock(up, dn) => {
      //println("in last process for assembly the result")
      if (matrixU.isEmpty) {
        matrixU = up
        matrixU = matrixU.addByCol(dn)
      }
      else {
        matrixU = matrixU.addByCol(up)
        matrixU = matrixU.addByCol(dn)
      }
      lastProcessNumber += 1
      if (lastProcessNumber == workNumber) {
        //println("Master: in ReturnedBlock")
        actorList.map(actor => context.stop(actor))
        println(s"sweep count is ${sweepCount}")
        mainThread ! matrixU
      }
    }

    case RoundRobinProcessDone => {
      iteration += 1
      if (iteration == workNumber * 2 - 1) {
        actorList.map(a => a ! QueryConverged)
        iteration = 0
        sweepCount += 1
        maxSteps -= 1
        if (maxSteps == 0) {
          actorList.map(actor => actor ! RetrieveBlocks)
          println("iteration exceed the max steps and forced quit")
        }
        println(s"Master: in RoundRobinProcessDone sweep count ${sweepCount}")
      }
      else
        actorList.map(a => a ! OutterOrth)
    }

    case HasDone => mainThread = sender
  }
}

class SvdWorker(id: Int) extends Actor {
  var upDataBlock = Matrix.emptyMatrix()
  var dnDataBlock = Matrix.emptyMatrix()
  var converged: Boolean = true
  var tol = 0.0
  var workerNumber = 0
  var iteration = 0
  val master = context.actorSelection("/user/master")

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
      val worker2 = context.actorSelection("/user/master/worker2")
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
            val worker = context.actorSelection("/user/master/worker" + (id - 1))
            worker ! new RoundRobin(upDataBlock, "up")
            upDataBlock = dataBlock
          }
        }
        case "dn" => {
          if (id == workerNumber) {
            val worker = context.actorSelection("/user/master/worker" + (id - 1))
            worker ! new RoundRobin(dnDataBlock, "up")
            dnDataBlock = dataBlock
          }
          else {
            val worker = context.actorSelection("/user/master/worker" + (id + 1))
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

    case InitialData(up, dn, value, workNumber) => {
      upDataBlock = up
      dnDataBlock = dn
      tol = value
      workerNumber = workNumber
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
      var c = 0.0
      var s = 0.0
      if (math.abs(dij) > tol)
        this.converged = false
      if (dij != 0) {
        val tao = (djj - dii) / (2 * dij)
        val t = math.signum(tao) / (math.abs(tao) + math.sqrt(math.pow(tao, 2) + 1))
        c = 1.0 / math.sqrt(math.pow(t, 2) + 1)
        s = t * c
        //update data block
        for (k <- 0 until block.row) {
          val res1 = block.get(k, i) * c - block.get(k, j) * s
          val res2 = block.get(k, i) * s + block.get(k, j) * c
          matrix.set(k, i, res1)
          matrix.set(k, j, res2)
        }
      }
/*      else {
        c = 1.0
        s = 0.0
      }*/
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
      var c = 0.0
      var s = 0.0
      if (math.abs(dij) > tol)
        this.converged = false
      if (dij != 0) {
        val tao = (djj - dii) / (2 * dij)
        val t = math.signum(tao) / (math.abs(tao) + math.sqrt(math.pow(tao, 2) + 1))
        c = 1.0 / math.sqrt(math.pow(t, 2) + 1)
        s = t * c

        //update data block
        for (k <- 0 until block1.row) {
          val res1 = block1.get(k, i) * c - block2.get(k, j) * s
          val res2 = block1.get(k, i) * s + block2.get(k, j) * c
          matrix1.set(k, i, res1)
          matrix2.set(k, j, res2)
        }
      } /*else {
        c = 1.0
        s = 0.0
      }*/
    }
    (matrix1, matrix2)
  }
}

object Svd {
  def main(args: Array[String]) {
    // args(0) is the worker number
    // args(1) and args(2) is the matrix row and col size
    if (args.size == 4) {
      val system = ActorSystem("ParallelSVD")
      val actor = system.actorOf(Props[SvdMaster], name = "master")
      val startTime = System.currentTimeMillis()
      val arg = args.map(x => x.toInt)
      actor ! new Initialization(arg(0), arg(1), arg(2), arg(3))
      implicit val timeout = Timeout(0 second)
      val future = actor ? HasDone
      val result = Await.result(future, timeout.duration).asInstanceOf[Matrix]
      actor ! PoisonPill
      val endTime = System.currentTimeMillis()
      println("time spend is " + (endTime - startTime) /  1000.0 + "s")
      //println(result.normalizeU.sliceByCol(0, result.row - 1))
      Thread.sleep(2000)
      system.shutdown()
    }
    else
      println("illegal parameters")
  }
}
