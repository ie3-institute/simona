/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.simona.api.data.connection.ExtResultDataConnection
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.api.ontology.results.{
  ProvideResultEntities,
  RequestResultEntities,
  ResultDataMessageFromExt,
}
import edu.ie3.simona.event.listener.DelayedStopHelper
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ResultMessage.{
  RequestResult,
  ResultResponse,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.util.CollectionUtils.asJava
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import scala.jdk.CollectionConverters.*

/** In contrast to the listener, the result provider will only provide those
  * result that were requested.
  */
object ExtResultProvider {

  type Message = ResultResponse | DelayedStopHelper.StoppingMsg

  /** State data for a result [[provider]].
    *
    * @param scheduler
    *   Reference to the scheduler.
    * @param resultProxy
    *   The result service proxy.
    * @param connection
    *   Result data connection to the external simulation.
    * @param lastTick
    *   The last tick for which results were requested.
    * @param extMessage
    *   Option for the current message from the external simulation.
    */
  private final case class ProviderState(
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[RequestResult],
      connection: ExtResultDataConnection,
      lastTick: Long,
      extMessage: Option[ResultDataMessageFromExt] = None,
  )

  /** Method to create an external result provider. In contrast to the listener,
    * the result provider will only provide those result that were requested.
    *
    * @param connection
    *   Result data connection to the external simulation.
    * @param scheduler
    *   Reference to the scheduler.
    * @param resultProxy
    *   The result service proxy.
    * @return
    *   The behavior of the result provider.
    */
  def apply(
      connection: ExtResultDataConnection,
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[RequestResult],
  ): Behavior[Message | DataMessageFromExt | Activation] = {
    val stateData =
      ProviderState(scheduler, resultProxy, connection, INIT_SIM_TICK)

    provider(stateData)
  }

  /** Definition of the behavior of the result provider.
    *
    * @param stateData
    *   The state data of the provider.
    * @return
    *   The behavior of the result provider.
    */
  private def provider(
      stateData: ProviderState
  ): Behavior[Message | DataMessageFromExt | Activation] =
    Behaviors.receivePartial[Message | DataMessageFromExt | Activation] {
      case (ctx, ResultResponse(results)) =>
        // send result to external simulation
        stateData.connection.handleResponseMsg(
          new ProvideResultEntities(results.asJava)
        )

        stateData.scheduler ! Completion(ctx.self)

        Behaviors.same

      case (_, messageFromExt: ResultDataMessageFromExt) =>
        // save ext message
        provider(stateData.copy(extMessage = Some(messageFromExt)))

      case (ctx, ScheduleServiceActivation(tick, unlockKey)) =>
        stateData.scheduler ! ScheduleActivation(
          ctx.self,
          tick,
          Some(unlockKey),
        )

        Behaviors.same

      case (ctx, Activation(tick)) =>
        // handle ext message

        val extMsg = stateData.extMessage.getOrElse(
          // this should not be possible because the external simulation schedules this provider
          throw CriticalFailureException(
            "ExtResultDataService was triggered without ResultDataMessageFromExt available"
          )
        )

        extMsg match {
          case requestResultEntities: RequestResultEntities =>
            val threshold = Option.when(
              !requestResultEntities.sendUnchangedResults
            )(stateData.lastTick)

            // request results from result proxy
            stateData.resultProxy ! RequestResult(
              requestResultEntities.requestedResults.asScala.toSeq,
              requestResultEntities.tick,
              ctx.self,
              threshold,
            )

            provider(stateData.copy(lastTick = tick))
          case other =>
            ctx.log.warn(s"Cannot handle external result message: $other")
            Behaviors.same
        }

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))

    }
}
