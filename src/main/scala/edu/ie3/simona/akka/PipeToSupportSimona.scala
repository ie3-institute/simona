/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.akka

import akka.actor.{ActorSystem, Status}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Adaptation of [[akka.pattern.PipeToSupport]] which is licensed under Apache
  * 2.0. TODO check how these licensing matters work
  */
object PipeToSupportSimona {

  /** Import this implicit conversion to gain the `pipeTo` method on
    * [[scala.concurrent.Future]]:
    *
    * {{{
    * import akka.pattern.pipe
    * // requires implicit ExecutionContext, e.g. by importing `context.dispatcher` inside an Actor
    *
    * Future { doExpensiveCalc() } pipeTo nextActor
    *
    * or
    *
    * pipe(someFuture) to nextActor
    *
    * }}}
    *
    * The successful result of the future is sent as a message to the recipient,
    * or the failure is sent in a [[akka.actor.Status.Failure]] to the
    * recipient.
    */
  implicit def pipe[T](
      future: Future[T]
  )(implicit executionContext: ExecutionContext): PipeableFuture[T] =
    new PipeableFuture(future)

  final class PipeableFuture[T](val future: Future[T])(implicit
      executionContext: ExecutionContext
  ) {
    def pipeTo(
        recipient: SimonaActorRef
    )(implicit system: ActorSystem): Future[T] = {
      future.andThen {
        case Success(r) => recipient ! r
        case Failure(f) => recipient ! Status.Failure(f)
      }
    }
  }
}
