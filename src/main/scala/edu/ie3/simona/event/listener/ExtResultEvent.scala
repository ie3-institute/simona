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
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ResultResponse
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{
  Activation,
  RequestResultMessage,
  SchedulerMessage,
}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import java.time.ZonedDateTime
import java.util
import java.util.UUID
import scala.jdk.CollectionConverters.*

object ExtResultEvent {

  type Message = ResultEvent.Response | DelayedStopHelper.StoppingMsg

  private final case class ProviderState(
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[RequestResultMessage],
      connection: ExtResultDataConnection,
      extMessage: Option[ResultDataMessageFromExt] = None,
      simStartTime: ZonedDateTime,
      gridAssets: List[UUID] = List.empty,
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
      resultProxy: ActorRef[RequestResultMessage],
      simStartTime: ZonedDateTime,
  ): Behavior[Message | DataMessageFromExt | Activation] = {
    val gridResults = connection.getGridResultDataAssets.asScala

    val stateData =
      ProviderState(
        scheduler,
        resultProxy,
        connection,
        simStartTime = simStartTime,
        gridAssets = gridResults.toList,
      )

    provider(stateData)
  }

  private def provider(
      stateData: ProviderState
  ): Behavior[Message | DataMessageFromExt | Activation] =
    Behaviors.receivePartial[Message | DataMessageFromExt | Activation] {
      case (ctx, ResultResponse(results)) =>
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

            if requestResultEntities.tick == 0 then {
              // removing the grid assets for tick 0, since SIMONA will produce no output
              requestedResults.removeAll(stateData.gridAssets.asJava)
            }

            // request results from result proxy
            stateData.resultProxy ! RequestResultMessage(
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
