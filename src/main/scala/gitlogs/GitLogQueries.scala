package gitlogs

import java.io.File

import scala.io.Source

import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._


/**
 * Created by olivertupran on 21/10/15.
 */
class GitLogQueries  {

  implicit val formats = DefaultFormats


  /**
   * Given a list of sources, countByField each source then aggregate the results.
   * This default implementation uses regexp for field value extraction... probably not very safe,
   * as the pattern itself is flimsy
   * @param sources
   * @param key
   * @return a map of field names and number of occurrences
   */
  def countByField(sources: List[Source], key: String) : Map[String, Int] = {
    countByField(regexpValueExtractor, sources, key)
  }

  /**
   * Given a value extractor, a list of sources, countByField each source then aggregate the results
   * @param extractor
   * @param sources
   * @param key
   * @return
   */
  def countByField(extractor: (String, String)  => String, sources: List[Source], key: String) : Map[String, Int] = {
    def fieldCounter = countByField(extractor)_
    lazy val allResults = sources.map(fieldCounter(_, key)).toStream
    lazy val allResultsAsList = allResults.map(_.toList).reduce(_ ++ _).toStream
    allResultsAsList.groupBy(p => p._1).map(p => (p._1, p._2.map(_._2))).map(p => (p._1, p._2.reduce(_ + _)))
  }

  /**
   * Given a value extractor, a source, parse each line as json, extract the type, group by type and count
   * @param extractor
   * @param source
   * @return a map of
   */
  def countByField(extractor: (String, String)  => String)(source: Source, key : String) : Map[String, Int] = {

    lazy val stream = source.getLines().toStream
    lazy val resultStream = stream.map(extractor(_, key))
    lazy val groupedStream = resultStream.groupBy(p => p)
    groupedStream.map{case (k: String, v: Stream[String]) => (k, v.size)}
  }


  def jsonValueExtractor(in: String, key: String): String = {
    (parse(in) \ key).extract[String]
  }

  def regexpValueExtractor(in: String, key: String): String = {
    val pattern = s""""$key":"(\\w*)"""".r
    pattern.findAllIn(in).matchData.toList.head.group(1)
  }

  def prettyPrintResults(results : Map[String, Int]) : Unit = {
    results.foreach(p => println("%-35s | %9d".format(p._1, p._2)))
  }

  def getFiles(dir: File): List[File] = dir.listFiles.toList

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Execution time: " + (t1 - t0) + " ms")
    result
  }

}
