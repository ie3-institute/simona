/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.simona.api.data.connection.{
  ExtResultDataConnection,
  ExtResultListener,
}
import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.api.ontology.results.{
  ProvideResultEntities,
  RequestResultEntities,
  ResultDataMessageFromExt,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ResultMessage.*
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{
  Activation,
  ResultMessage,
  SchedulerMessage,
}
import edu.ie3.simona.util.CollectionUtils.asJava
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.util
import scala.jdk.CollectionConverters.*

object ExtResultEvent {

  type Message = ResultMessage.Response | DelayedStopHelper.StoppingMsg

  private final case class ProviderState(
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[RequestResult],
      connection: ExtResultDataConnection,
      extMessage: Option[ResultDataMessageFromExt] = None,
  )

  def listener(connection: ExtResultListener): Behavior[Message] =
    Behaviors.receivePartial[Message] {
      case (_, ResultResponse(results)) =>
        connection.queueExtResponseMsg(
          new ProvideResultEntities(results.asJava)
        )

        Behaviors.same

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))
    }

  def provider(
      connection: ExtResultDataConnection,
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[RequestResult],
  ): Behavior[Message | DataMessageFromExt | Activation] = {
    val stateData = ProviderState(scheduler, resultProxy, connection)

    provider(stateData)
  }

  private def provider(
      stateData: ProviderState
  ): Behavior[Message | DataMessageFromExt | Activation] =
    Behaviors.receivePartial[Message | DataMessageFromExt | Activation] {
      case (ctx, ResultResponse(results)) =>
        ctx.log.warn(s"Sending results to ext. Results: $results")

        // send result to external simulation
        stateData.connection.queueExtResponseMsg(
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
            val requestedResults =
              new util.ArrayList(requestResultEntities.requestedResults)

            // request results from result proxy
            stateData.resultProxy ! RequestResult(
              requestedResults.asScala.toSeq,
              tick,
              ctx.self,
            )

            Behaviors.same
          case other =>
            ctx.log.warn(s"Cannot handle external result message: $other")
            Behaviors.same
        }

      case (ctx, msg: DelayedStopHelper.StoppingMsg) =>
        DelayedStopHelper.handleMsg((ctx, msg))

    }
}
