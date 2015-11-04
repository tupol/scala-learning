import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by olivertupran on 23/10/15.
 */
package object futures {


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



}
