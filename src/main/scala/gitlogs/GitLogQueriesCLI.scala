package gitlogs

import java.io.File

import scala.io.Source

/**
 * Created by olivertupran on 22/10/15.
 */
class GitLogQueriesCLI  extends App {


  override def main(args: Array[String]) = {

    val dir = new File("../glogs/")

    val glq = new GitLogQueries

    val sources = glq.getFiles(dir).map(Source.fromFile(_)).take(10)

    println(s"Processing ${sources.size} files ...")

    glq.time(glq.prettyPrintResults(glq.countByField(sources, "type")))

    sources.map(_.close())

  }


}