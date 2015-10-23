package gitlogs

import java.io.{File}
import java.nio.file.{Paths, Path}

import scala.concurrent.Future
import scala.io.Source


import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}


/**
 * Created by olivertupran on 21/10/15.
 */
object GitLogQueries  {

  // TODO: Try also an akk implementation

  // TODO: Think about this:
  // I can argue that the Result can be a type in itself and should have "composition" methods (or operators)
  // like add, subtract...
  // This wold make results summing up far more easier and more elegant.
  // However, at this point I am not completely happy with the structure, so it is postponed.
  type Result = Map[String, Int]

  /**
   * Calculate the result using one Future for each Path
   * @param paths
   * @param key
   * @return
   */
  def calculateInFutures(paths : List[Path], key: String) : Future[Result] = {

    def processor(Path: Path) = Future { calculatePerPath(Path, key) }
    val futures = paths.map(processor(_))
    val joinedFutures = joinFutures(futures).map(aggregateTryResults(_))
    // maybe someday this line will look much cooler
//    val joinedFutures = futures.foldRight(Future(List[Try[Result]]()))((f, acc) => f addList acc)
    joinedFutures

  }

  /**
   * Calculate result in one Future for the entire input list
   * @param paths
   * @param key
   * @return
   */
  def calculateInFuture(paths : List[Path], key: String) : Future[Result] = Future {
    val partial = paths.map(calculatePerPath(_, key))
    aggregateTryResults(partial)
  }

  /**
   * Calculate the final result synchronously
   * @param paths
   * @param key
   * @return
   */
  def calculate(paths : List[Path], key: String) : Result = {
    val partial = paths.map(calculatePerPath(_, key))
    aggregateTryResults(partial)
  }

  /**
   * Sum up a lit of Results of Try into a Result, adding the values for each int value, and ignoring the failures
   * @param list
   * @return
   */
  def aggregateTryResults(list: List[Try[Result]]) : Result = {
    aggregateResults(list
      // get only successful results; maybe in the future we do something with the failures (like logging)
      .filter(_.isSuccess).map(_.get))
  }


  /**
   * Sum up a lit of map into a map, adding the values for each int value
   * @param list
   * @return
   */
  def aggregateResults(list: List[Result]) : Result = {
    list
      // create one big happy list of tuples
      .flatMap(_.toList)
      // group the list by the first element of the tuple (the name)
      .groupBy(p => p._1)
      // and now for a bit of magic...
      .map(p =>
        (p._1,
          p._2.foldLeft(0)((acc, tup) => acc + tup._2)))
  }

  /**
   * CalculatePerPath using the 'RegExpExtractor'
   * @param Path
   * @param key
   * @return
   */
  def calculatePerPath(Path : Path, key : String): Try[Result] = {
    calculatePerPath(RegexpFieldExtractor.extract(key))(Path, key)
  }

  /**
   * Given a value extractor, a Path, parse each line as json, extract the type, group by type and count
   * @param extract
   * @param path
   * @return a map of
   */
  def calculatePerPath(extract: (String)  => Option[String])(path: Path, key : String) : Try[Result] = Try {

    //TODO probably this should return a Try[Map....]
    def source = Source.fromFile(path.toUri)
    def lines = source.getLines()
    def resultList = lines.map(extract(_)).toList
    // Let's count the "non-occurrences as well"
    def resultsWithNones = resultList.map { case Some(x) => x; case None => "{NONE}" }
    def groupedList = resultsWithNones.groupBy(p => p)
    val result = groupedList.map{case (k: String, v: List[String]) => (k, v.size)}
    source.close()
    result
  }

  def getPaths(dir: File): List[Path] =
    dir.listFiles.toList
      .filter(f => f.getName.toLowerCase.endsWith(".json"))
      .map(f => Paths.get(f.toURI))

  /** Adds extension methods to future objects.
    */
  implicit class FutureOps[T](val self: Future[T]) extends AnyVal {
    def add(that : Future[T]) : Future[List[T]] = {
      self.flatMap(x => that.map(y => List(x, y)))
    }
    def addList(that : Future[List[T]]) : Future[List[T]] = {
      self.flatMap(x => that.map(y => x :: y))
    }
  }

  /**
   * Given a list of futures, return a future of list
   * @param list of futures
   * @tparam T type of the Future
   * @return a Future of list of T
   */
  def joinFutures[T](list : Seq[Future[T]]) : Future[List[T]] = list match {
    case Nil => Future(List[T]())
    case head :: tail => head.flatMap(x => joinFutures(tail).map(xs => x :: xs))
  }

}

