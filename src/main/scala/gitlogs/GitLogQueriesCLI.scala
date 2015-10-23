
import language.postfixOps
import java.io.File
import gitlogs._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.file._

/**
 * Created by olivertupran on 22/10/15.
 */
object GitLogQueriesCLI extends App {


  // TODO add parameters parsing...
  override def main(args: Array[String]) = {

    import GitLogQueries._

    val dir = new File("../glogs/")

    val paths = getPaths(dir)
//      .take(200)

//    println("-----------------------------------")
//    println(s"Processing ${paths.size} paths synchronously...")
//
//    time {
//      val result = calculate(paths, "type")
//      prettyPrintResults(result)
//    }


//    println("-----------------------------------")
//    println(s"Processing ${paths.size} paths using one Future per file...")
//
//    time {
//      val futureResult = calculateInFutures(paths, "type")
//      val result = Await.result(futureResult, 600 seconds)
//      prettyPrintResults(result)
//    }

    println("-----------------------------------")
    println(s"Processing ${paths.size} paths using ${paths.size / 8} Futures...")

    time {
      val splitNo = paths.size / 8
      val sFiles = splitList(paths, splitNo)
      val futureResults = sFiles.map((calculateInFuture(_, "type")))
      //      val futureResult = futureResult1.flatMap(r1 => futureResult2.map(r2 => List(r1, r2)))
      val futureResult = joinFutures(futureResults)
      val result = Await.result(futureResult, 600 seconds)
      prettyPrintResults(aggregateResults(result))
    }


//    println("-----------------------------------")
//    println(s"Processing ${paths.size} paths ...")
//
//    time {
//      val result = calculate(paths, "type")
//      prettyPrintResults(result)
//    }

    def splitList[T](list: List[T], parts: Int): List[List[T]] = parts match {
      case 1 => List(list)
      case _ => {
        val sl = list.splitAt(list.size/parts)
        sl._1 :: splitList(sl._2, parts - 1)
      }
    }
  }

  def prettyPrintResults(results : Map[String, Int]) : Unit = {
    results.foreach(p => println("%-35s | %9d".format(p._1, p._2)))
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Execution time: " + (t1 - t0) + " ms")
    result
  }



}

