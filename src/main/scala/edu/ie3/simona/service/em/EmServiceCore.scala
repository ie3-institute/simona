/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.api.data.em.model.NoSetPointValue
import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.simona.ontology.messages.services.EmMessage.{
  WrappedFlexRequest,
  WrappedFlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  RegisterForEmDataService,
  ServiceResponseMessage,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.Power
import squants.energy.Kilowatts
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Power => PsdmPower}
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional

trait EmServiceCore {
  def lastFinishedTick: Long

  def uuidToFlexAdapter: Map[UUID, ActorRef[FlexRequest]]

  implicit class SquantsToQuantity(private val value: Power) {
    def toQuantity: ComparableQuantity[PsdmPower] = value.toMegawatts.asMegaWatt
  }
  implicit class QuantityToSquants(
      private val value: ComparableQuantity[PsdmPower]
  ) {
    def toSquants: Power = Kilowatts(
      value.to(PowerSystemUnits.KILOWATT).getValue.doubleValue()
    )
  }

  def handleRegistration(
      registerForEmDataService: RegisterForEmDataService
  ): EmServiceCore

  def handleExtMessage(
      tick: Long,
      extMSg: EmDataMessageFromExt,
  )(implicit
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])

  final def handleDataResponseMessage(
      tick: Long,
      responseMsg: ServiceResponseMessage,
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = responseMsg match {
    case WrappedFlexRequest(flexRequest, receiver) =>
      handleFlexRequest(flexRequest, receiver)

    case WrappedFlexResponse(flexResponse, receiver) =>
      handleFlexResponse(tick, flexResponse, receiver)
  }

  final def handleSetPoint(
      tick: Long,
      provideEmSetPoints: ProvideEmSetPointData,
      log: Logger,
  ): Unit = {
    log.info(s"Handling of: $provideEmSetPoints")

    provideEmSetPoints
      .emData()
      .asScala
      .foreach { case (agent, setPoint) =>
        uuidToFlexAdapter.get(agent) match {
          case Some(receiver) =>
            setPoint match {
              case _: NoSetPointValue =>
                receiver ! IssueNoControl(tick)
              case _ =>
                val power =
                  setPoint.getP.toScala.map(_.toSquants).getOrElse(zeroKW)

                receiver ! IssuePowerControl(tick, power)
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
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])

  def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])
}
