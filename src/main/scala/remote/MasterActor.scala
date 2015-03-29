package remote

/**
 * Created on 15/3/29.
 */

import akka.actor._
import datatype.Matrix

class MasterActor extends Actor{
  var workNumber = 0
  var matrix = Matrix.emptyMatrix()
  var tol = 0d
  var firstProcessNumber = 0
  var outterOrthNumber = 0
  var secondProcessNumber = 0
  var iteration = 0
  var allConverged = true
  var actorList: List[ActorRef] = Nil
  var actorPathList: List[String] = Nil
  var mainThread: ActorRef = _
  var lastProcessNumber = 0
  var matrixU = Matrix.emptyMatrix()
  var sweepCount = 0
  var maxSteps = 75

  def receive = {
    case Initialization(n, row, col) => {
      //println("in master initialization")
      //split a matrix by columns with equal size
      workNumber = n
      matrix = Matrix.random(row, col)
      tol = math.pow(matrix.norm, 2) * 1e-15

      println(s"initialized matrix size is ${matrix.row} x ${matrix.col}")

      for (i <- 1 to workNumber) {
        val actor = context.actorOf(Props(classOf[WorkerActor], i), name = "worker" + i)
        val path = actor.path.toString
        actorList = actor :: actorList
        actorPathList = path :: actorPathList
      }
      actorPathList = actorPathList.reverse
      actorList = actorList.reverse
      actorPathList.foreach(str => println(str))

      val blockList = matrix.sliceByColEqually(workNumber)
      //println(matrix)
      //blockList.foreach(m => println(m))
      for ((actor, i) <- actorList.zipWithIndex) {
        val block = blockList(i).sliceByColEqually(2)
        actor ! new InitialData(block(0), block(1), tol, actorPathList)
        actor ! InnerOrth
      }
    }
    case InnerOrthDone => {
      firstProcessNumber += 1
      if (firstProcessNumber == workNumber) {
        firstProcessNumber = 0
        actorList.foreach(a => a ! OutterOrth)
        //println("Master: in InnerOrthDone")
      }
    }

    case OutterOrthDone => {
      outterOrthNumber += 1
      if (outterOrthNumber == workNumber) {
        outterOrthNumber = 0
        val worker = actorList.head
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
