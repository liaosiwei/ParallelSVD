package remote

import akka.actor.{Props, ActorSystem, PoisonPill}
import akka.util.Timeout
import remote.MessageType.{HasDone, Initialization}
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import datatype.Matrix

/**
 * Created on 15/3/29.
 */
object SvdApp {
  def main(args: Array[String]) {
    // args(0) is the worker number
    // args(1) and args(2) is the matrix row and col size
    if (args.head == "worker") {
      startWorkerSystem()
    }
    else if ((args.head == "master") && (args.size == 4)) {
      // args(0): trigger which actor type
      // args(1): number of workers which must agree with the deployed worker number in master.conf
      // args(2), args(3): the matrix row and column
      startMasterSystem(args.tail)
    }
    else
      println("illegal parameters")
  }
  def startWorkerSystem() = {
    ActorSystem("WorkerSystem", ConfigFactory.load("worker"))
    println("Started WorkerSystem.")
  }

  def startMasterSystem(args: Array[String]) = {
    val system = ActorSystem("ParallelSvd", ConfigFactory.load("master"))
    val actor = system.actorOf(Props[MasterActor], name = "master")
    val startTime = System.currentTimeMillis()
    val arg = args.map(x => x.toInt)
    actor ! new Initialization(arg(0), arg(1), arg(2))
    implicit val timeout = Timeout(5 day)
    val future = actor ? HasDone
    val result = Await.result(future, timeout.duration).asInstanceOf[Matrix]
    actor ! PoisonPill
    val endTime = System.currentTimeMillis()
    println("time spend is " + (endTime - startTime) /  1000.0 + "s")
    val res = result.normalizeU.sliceByCol(0, result.row - 1)
    Thread.sleep(2000)
    system.shutdown()
  }
}
