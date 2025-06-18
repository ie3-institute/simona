/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.{PValue, SValue}
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.em.ontology.*
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  EmFlexMessage,
  EmServiceRegistration,
  ServiceResponseMessage,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.util.quantities.QuantityUtils.asMegaWatt
import edu.ie3.util.scala.quantities.QuantityConversionUtils.PowerConversionSimona
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.Power
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Power as PsdmPower
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional

trait EmServiceCore {
  def lastFinishedTick: Long

  def uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]]

  def completions: ReceiveDataMap[UUID, FlexCompletion]

  implicit class SquantsToQuantity(private val value: Power) {
    def toQuantity: ComparableQuantity[PsdmPower] = value.toMegawatts.asMegaWatt
  }

  def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceCore

  def handleExtMessage(
      tick: Long,
      extMSg: EmDataMessageFromExt,
  )(using
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])

  final def handleDataResponseMessage(
      tick: Long,
      responseMsg: ServiceResponseMessage,
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = responseMsg match {
    case EmFlexMessage(flexRequest: FlexRequest, receiver) =>
      receiver match {
        case ref: ActorRef[FlexRequest] =>
          handleFlexRequest(flexRequest, ref)

        case _ =>
          // should not happen
          log.warn(s"No receiver found for msg: $flexRequest")
          (this, None)
      }

    case EmFlexMessage(flexResponse: FlexResponse, receiver) =>
      receiver match {
        case uuid: UUID =>
          handleFlexResponse(tick, flexResponse, Left(uuid))

        case ref: ActorRef[FlexResponse] =>
          handleFlexResponse(tick, flexResponse, Right(ref))
      }
  }

  final def handleSetPoint(
      tick: Long,
      provideEmSetPoints: ProvideEmSetPointData,
      log: Logger,
  ): Unit = {
    log.info(s"Handling of: $provideEmSetPoints")

    provideEmSetPoints.emSetPoints.asScala
      .foreach { case (agent, setPoint) =>
        uuidToAgent.get(agent) match {
          case Some(receiver) =>
            val (pOption, qOption) = setPoint.power.toScala match {
              case Some(sValue: SValue) =>
                (sValue.getP.toScala, sValue.getQ.toScala)
              case Some(pValue: PValue) =>
                (pValue.getP.toScala, None)
              case None =>
                (None, None)
            }

            (pOption, qOption) match {
              case (Some(activePower), _) =>
                receiver ! IssuePowerControl(tick, activePower.toSquants)

              case (None, _) =>
                receiver ! IssueNoControl(tick)
            }

          case None =>
            log.warn(s"No em agent with uuid '$agent' registered!")
        }
      }

  }

  def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])

  def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])

  final def getMaybeNextTick: Option[java.lang.Long] = completions.receivedData
    .flatMap { case (_, completion) =>
      completion.requestAtTick
    }
    .minOption
    .map(long2Long)
}
