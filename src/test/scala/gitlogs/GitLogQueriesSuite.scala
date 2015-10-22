package gitlogs

import java.io.File

import org.scalatest.FunSuite

import scala.io.Source

/**
 * Created by olivertupran on 22/10/15.
 */
class GitLogQueriesSuite extends FunSuite {


  trait GitLogQueriesTest extends GitLogQueries {

    val dir = new File("src/test/resources/")

  }

  test("simple test") {

    new GitLogQueriesTest {

      val expected = Map("PushEvent" -> 4280, "WatchEvent" -> 642, "CreateEvent" -> 815, "MemberEvent" -> 16,
        "PullRequestReviewCommentEvent" -> 85, "PublicEvent" -> 2, "IssueCommentEvent" -> 650, "GollumEvent" -> 90,
        "PullRequestEvent" -> 315, "ForkEvent" -> 213, "CommitCommentEvent" -> 56, "DeleteEvent" -> 141,
        "IssuesEvent" -> 373, "ReleaseEvent" -> 24)

      val sources = getFiles(dir).map(Source.fromFile(_))

      val result = (countByField(sources, "type"))

      assert(result === expected)

      sources.map(_.close())
    }
  }

}
