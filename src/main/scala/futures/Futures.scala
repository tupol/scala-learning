package futures

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by olivertupran on 23/10/15.
 */
case class Futures[T](futures : Future[List[T]]) {



//  def :+ (future: Future[T]) = {
//    futures + future
//  }
//
//
//  def +: (future: Future[T]) = {
//    Futures(future.addList(futures))
//  }

  /**
   * Given a list of futures, return a future of list
   * @param list of futures
   * @tparam T type of the Future
   * @return a Future of list of T
   */
  def joinFutures[T](list : List[Future[T]]) : Future[List[T]] = list match {
    case Nil => Future(List[T]())
    case head :: tail => head.flatMap(x => joinFutures(tail).map(xs => x :: xs))
  }

}
