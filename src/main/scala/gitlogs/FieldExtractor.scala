package gitlogs


import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods._



/**
 * Created by olivertupran on 23/10/15.
 */
trait FieldExtractor {

  def extract(name: String)(input : String): Option[String]

}

object JsonFieldExtractor extends FieldExtractor {

  implicit val formats = DefaultFormats

  def extract(name: String)(input: String): Option[String] = {
    (parse(input) \ name).toSome.map(_.extract[String])
  }

}

object RegexpFieldExtractor extends FieldExtractor {

  def extract(name: String)(input: String): Option[String] =  {
    val pattern = s""""$name":"(\\w*)"""".r
    pattern.findFirstMatchIn(input).map(_.group(1))
  }
}

