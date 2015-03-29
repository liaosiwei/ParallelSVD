package remote

import datatype.Matrix

/**
 * Created on 15/3/29.
 */

case class Initialization(workerNumber: Int, row: Int, col: Int)
case class InitialData(upBlock: Matrix, dnBlock: Matrix, tol: Double, actorPathList: List[String])

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


