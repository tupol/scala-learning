package com.lunatech.gitlogs

import java.io.File

//import scala.collection.JavaConversions._
import scala.io.{BufferedSource, Source}

import org.json4s
import org.json4s.{DefaultFormats, Formats}
import org.json4s.jackson.JsonMethods._


/**
 * Created by olivertupran on 21/10/15.
 */
object GitLogQueries extends App {

  implicit val formats = DefaultFormats

  override def main(args: Array[String]): Unit = {

    val dir = new File("src/test/resources/")
    val dir = new File("src/test/resources/")

    lazy val sources = getFiles(dir).take(20).toParArray.map(Source.fromFile(_)).toList

    time(countByField(sources, "type").foreach(p => println("%-35s | %6d".format(p._1, p._2))))

    sources.map(_.close())

  }

  /**
   * Given a list of sources, countByField each source then aggregate the results
   * @param sources
   * @param key
   * @return a map of field names and number of occurrences
   */
  def countByField(sources: List[Source], key: String) : Map[String, Int] = {
    lazy val allResults = sources.map(countByField(_, key)).toStream
    lazy val allResultsAsList = allResults.map(_.toList).reduce(_ ++ _).toStream
    allResultsAsList.groupBy(p => p._1).map(p => (p._1, p._2.map(_._2))).map(p => (p._1, p._2.reduce(_ + _)))
  }

  /**
   * Given a source, parse each line as json, extract the type, group by type and count
   * @param source
   * @return a map of
   */
  def countByField(source: Source, key : String) : Map[String, Int] = {
    lazy val stream = source.getLines().map(parse(_)).toStream
    lazy val resultStream = stream.map(j => compact(j \ key))
    lazy val groupedStream = resultStream.groupBy(p => p)
    groupedStream.map{case (k: String, v: Stream[String]) => (k, v.size)}
  }

  def getFiles(dir: File): List[File] = dir.listFiles.toList


  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Execution time: " + (t1 - t0) + " ms")
    result
  }

}
