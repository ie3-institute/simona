/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.datamodel.models.value.{PValue, SValue}
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.FlexConversion
import edu.ie3.simona.api.data.model.em.{EmSetPoint, FlexOptions}
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  EmFlexMessage,
  EmServiceRegistration,
  ServiceResponseMessage,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.quantities.QuantityUtils.asMegaWatt
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.Power
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.{Optional, UUID}
import javax.measure.quantity.Power as PsdmPower
import scala.jdk.OptionConverters.{RichOption, RichOptional}

/** Trait for all em service cores.
  */
trait EmServiceCore {

  /** Map: uuid to em agent reference.
    */
  val uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]]

  val agentToUuid: Map[ActorRef[FlexRequest] | ActorRef[FlexResponse], UUID]

  val uncontrolled: Set[UUID]

  val uuidToInferior: Map[UUID, Set[UUID]]

  val uuidToParent: Map[UUID, UUID]

  /** Map: uuid to flex option result.
    */
  val allFlexOptions: Map[UUID, FlexOptions]

  /** ReceiveDataMap: uuid to completions.
    */
  val completions: ReceiveDataMap[UUID, FlexCompletion]

  val nextActivation: Map[UUID, Long]

  /** Extension to convert a squants power value to a psdm power value.
    */
  extension (value: Power) {
    def toQuantity: ComparableQuantity[PsdmPower] = value.toMegawatts.asMegaWatt
  }

  given Conversion[Optional[java.lang.Long], Option[Long]] =
    (x: Optional[java.lang.Long]) => x.toScala.map(Long2long)
  given Conversion[Option[Long], Optional[java.lang.Long]] =
    (x: Option[Long]) => x.map(long2Long).toJava

  /** Method to handle a registration message.
    *
    * @param emServiceRegistration
    *   The registration to handle.
    * @return
    *   An updated service core.
    */
  def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceCore

  /** Method to handle the received message from the external simulation.
    * @param tick
    *   Current tick of the service.
    * @param extMsg
    *   The message from external.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  def handleExtMessage(
      tick: Long,
      extMsg: EmDataMessageFromExt,
  )(using
      log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt])

  /** Method to handle data response messages from the em agents.
    * @param tick
    *   Current tick of the service.
    * @param responseMsg
    *   To handle.
    * @param startTime
    *   The start time of the simulation.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  final def handleDataResponseMessage(
      tick: Long,
      responseMsg: ServiceResponseMessage,
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = responseMsg match {
    case EmFlexMessage(flexRequest: FlexRequest, receiver) =>
      log.debug(s"$receiver <- $flexRequest")

      receiver match {
        case ref: ActorRef[FlexRequest] =>
          if tick == INIT_SIM_TICK then {
            ref ! flexRequest

            (this, None)
          } else {
            handleFlexRequest(flexRequest, ref)
          }

        case _ =>
          // should not happen
          log.warn(s"No receiver found for msg: $flexRequest")
          (this, None)
      }

    case EmFlexMessage(flexResponse: FlexResponse, receiver) =>
      log.debug(s"$receiver <- $flexResponse")

      receiver match {
        case uuid: UUID =>
          handleFlexResponse(tick, flexResponse, Left(uuid))

        case ref: ActorRef[FlexResponse] =>
          if tick == INIT_SIM_TICK then {
            ref ! flexResponse
            (this, None)
          } else {
            handleFlexResponse(tick, flexResponse, Right(ref))
          }
      }
  }

  /** Method to handle the set points provided by the external simulation.
    * @param tick
    *   Current tick of the service.
    * @param setPoints
    *   The set points to handle.
    * @param log
    *   Logger for logging messages.
    */
  final def handleSetPoint(
      tick: Long,
      setPoints: Map[UUID, EmSetPoint],
      log: Logger,
  ): Unit = {
    setPoints.foreach { case (agent, setPoint) =>
      uuidToAgent.get(agent) match {
        case Some(receiver) =>
          receiver ! FlexConversion.convert(tick, setPoint)

        case None =>
          log.warn(s"No em agent with uuid '$agent' registered!")
      }
    }
  }

  /** Method to handle flex responses from the em agents.
    * @param tick
    *   Current tick of the service.
    * @param flexResponse
    *   From the agent to handle.
    * @param receiver
    *   The receiver of the agent.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt])

  /** Method to handle flex requests to the em agents.
    * @param flexRequest
    *   That is sent to an agents.
    * @param receiver
    *   Of the flex request.
    * @param log
    *   Logger for logging messages.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using log: Logger): (EmServiceCore, Option[EmDataResponseMessageToExt])

  /** Method to handle received completion messages.
    * @param tick
    *   Current tick of the service.
    * @param completion
    *   To handle.
    * @return
    *   The updated ReceiveDataMap an option for or a message that should be
    *   sent to the external simulation and a boolean that tells, if all
    *   completions have been received.
    */
  final def handleCompletion(tick: Long, completion: FlexCompletion): (
      ReceiveDataMap[UUID, FlexCompletion],
      Option[EmDataResponseMessageToExt],
      Option[Long],
      Boolean,
  ) = {
    val updated = completions.addData(completion.modelUuid, completion)

    if updated.isComplete then {
      val (extMsgOption, nextTickOption) = if tick != INIT_SIM_TICK then {
        // send completion message to external simulation, if we aren't in the INIT_SIM_TICK
        val option = getMaybeNextTick

        (Some(new EmCompletion(option)), option)
      } else (None, None)

      // every em agent has sent a completion message
      (updated, extMsgOption, nextTickOption, true)

    } else (updated, None, None, false)
  }

  /** Method to calculate the next tick option.
    * @return
    *   An option for the next activation tick.
    */
  final def getMaybeNextTick: Option[Long] =
    completions.receivedData.flatMap { case (_, completion) =>
      completion.requestAtTick
    }.minOption
}
