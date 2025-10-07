/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.api.data.model.em.ExtendedFlexOptionsResult
import edu.ie3.simona.api.ontology.em.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.ServiceMessage.EmServiceRegistration
import edu.ie3.simona.ontology.messages.flex.FlexType.PowerLimit
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.flex.PowerLimitFlexOptions
import edu.ie3.simona.util.ReceiveDataMap
import edu.ie3.simona.util.SimonaConstants.{INIT_SIM_TICK, PRE_INIT_TICK}
import edu.ie3.simona.util.TickUtil.TickLong
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.{
  ListHasAsScala,
  MapHasAsJava,
  SetHasAsScala,
}
import scala.jdk.OptionConverters.RichOption

/** Basic service core for an [[ExtEmDataService]].
  * @param lastFinishedTick
  *   The last tick that was completed.
  * @param uuidToAgent
  *   Map: uuid to em agent reference.
  * @param flexOptions
  *   ReceiveDataMap: uuid to flex option result.
  * @param allFlexOptions
  *   Map: uuid to flex option result.
  * @param completions
  *   ReceiveDataMap: uuid to completions.
  * @param structure
  *   Map: uuid to inferior em agent uuids.
  * @param disaggregatedFlex
  *   True, if disaggregated flex options should be sent to the external
  *   simulation.
  * @param sendOptionsToExt
  *   True, if flex options should be sent to the external simulation.
  * @param canHandleSetPoints
  *   True, if the core can process the em set points.
  * @param setPointOption
  *   Option for em set points that needs to be handled at a later time.
  */
final case class EmServiceBaseCore(
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToAgent: Map[UUID, ActorRef[EmAgent.Message]] = Map.empty,
    flexOptions: ReceiveDataMap[UUID, ExtendedFlexOptionsResult] =
      ReceiveDataMap.empty,
    override val allFlexOptions: Map[UUID, ExtendedFlexOptionsResult] =
      Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    structure: Map[UUID, Set[UUID]] = Map.empty,
    disaggregatedFlex: Boolean = false,
    sendOptionsToExt: Boolean = false,
    canHandleSetPoints: Boolean = false,
    setPointOption: Option[ProvideEmSetPointData] = None,
) extends EmServiceCore {

  override def handleRegistration(
      emServiceRegistration: EmServiceRegistration
  ): EmServiceBaseCore = {
    val ref = emServiceRegistration.requestingActor
    val modelUuid = emServiceRegistration.inputUuid
    val parentUuid = emServiceRegistration.parentUuid

    val updatedStructure = parentUuid match {
      case Some(parent) =>
        structure.get(parent) match {
          case Some(subEms) =>
            val allSubEms = subEms + modelUuid
            structure ++ Map(parent -> allSubEms)
          case None =>
            structure ++ Map(parent -> Set(modelUuid))
        }

      case _ =>
        // we already added the model as parent
        // therefore, no changes are needed
        structure
    }

    copy(
      uuidToAgent = uuidToAgent + (modelUuid -> ref),
      completions = completions.addExpectedKeys(Set(modelUuid)),
      structure = updatedStructure ++ Map(modelUuid -> Set.empty[UUID]),
    )
  }

  override def handleExtMessage(tick: Long, extMsg: EmDataMessageFromExt)(using
      log: Logger
  ): (EmServiceBaseCore, Option[EmDataResponseMessageToExt]) = extMsg match {
    case requestEmFlexResults: RequestEmFlexResults =>
      val tick = requestEmFlexResults.tick
      val emEntities = requestEmFlexResults.emEntities.asScala
      val disaggregated = requestEmFlexResults.disaggregated

      if disaggregated then {
        log.warn(s"Disaggregated flex options are currently not supported!")
      }

      emEntities.map(uuidToAgent).foreach { ref =>
        ref ! FlexActivation(tick, PowerLimit)
      }

      (
        copy(
          flexOptions = ReceiveDataMap(emEntities.toSet),
          disaggregatedFlex = disaggregated,
          sendOptionsToExt = true,
        ),
        None,
      )

    case provideEmSetPoints: ProvideEmSetPointData =>
      if canHandleSetPoints then {
        handleSetPoint(tick, provideEmSetPoints, log)

        (this, None)
      } else {
        val tick = provideEmSetPoints.tick
        val emEntities = provideEmSetPoints.emSetPoints.keySet.asScala

        emEntities.foreach { entity =>
          uuidToAgent.get(entity) match {
            case Some(ref) =>
              ref ! FlexActivation(tick, PowerLimit)
            case None =>
              log.warn(s"Received entity: $entity")
          }
        }

        (
          copy(
            flexOptions = ReceiveDataMap(emEntities.toSet),
            setPointOption = Some(provideEmSetPoints),
          ),
          None,
        )
      }

    case _ =>
      throw new CriticalFailureException(
        s"The EmServiceBaseCore is not able to handle the message: $extMsg"
      )
  }

  override def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceBaseCore, Option[EmDataResponseMessageToExt]) = {
    receiver.foreach(_ ! flexResponse)

    flexResponse match {
      case provideFlexOptions: ProvideFlexOptions =>
        val (updated, updatedAdditional) =
          handleFlexOptions(tick, provideFlexOptions)

        if updated.isComplete then {
          // we received all flex options
          val data = updated.receivedData

          if disaggregatedFlex then {
            // we add the disaggregated flex options
            addDisaggregatingFlexOptions(data, structure)
          }

          val updatedCore = copy(
            flexOptions = ReceiveDataMap.empty,
            allFlexOptions = updatedAdditional,
            canHandleSetPoints = true,
          )

          if sendOptionsToExt then {
            // we have received an option request, that will now be answered
            (updatedCore, Some(new FlexOptionsResponse(data.asJava)))

          } else {
            setPointOption match {
              case Some(setPoints) =>
                // we have received new set points, that are not handled yet => we will handle them now
                handleSetPoint(tick, setPoints, log)

                (updatedCore.copy(setPointOption = None), None)
              case None =>
                // we are now able to handle set points, but we have not yet received any
                (updatedCore, None)
            }
          }

        } else {
          (
            copy(
              flexOptions = updated,
              allFlexOptions = updatedAdditional,
            ),
            None,
          )
        }

      case completion: FlexCompletion =>
        val (updated, extMsgOption, finished) =
          handleCompletion(tick, completion)

        if finished then {
          (
            copy(
              lastFinishedTick = tick,
              completions = updated,
              allFlexOptions = Map.empty,
              disaggregatedFlex = false,
              sendOptionsToExt = false,
              canHandleSetPoints = false,
            ),
            extMsgOption,
          )

        } else (copy(completions = updated), extMsgOption)

      case _ =>
        (this, None)
    }
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(using
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceBaseCore, Option[EmDataResponseMessageToExt]) = {
    log.debug(s"$receiver: $flexRequest")
    receiver ! flexRequest

    (this, None)
  }

  /** Method to handle flex options.
    * @param tick
    *   Current tick of the service.
    * @param provideFlexOptions
    *   The provided flex options.
    * @param startTime
    *   The start time of the simulation.
    * @return
    *   An updated service core and an option for a message that should be sent
    *   to the external simulation.
    */
  private def handleFlexOptions(
      tick: Long,
      provideFlexOptions: ProvideFlexOptions,
  )(using startTime: ZonedDateTime): (
      ReceiveDataMap[UUID, ExtendedFlexOptionsResult],
      Map[UUID, ExtendedFlexOptionsResult],
  ) = provideFlexOptions match {
    case ProvideFlexOptions(
          modelUuid: UUID,
          PowerLimitFlexOptions(ref, min, max),
        ) =>
      val result = new ExtendedFlexOptionsResult(
        tick.toDateTime,
        modelUuid,
        modelUuid,
        min.toQuantity,
        ref.toQuantity,
        max.toQuantity,
      )

      if flexOptions.getExpectedKeys.contains(modelUuid) then {
        (
          flexOptions.addData(modelUuid, result),
          allFlexOptions,
        )
      } else {
        (
          flexOptions,
          allFlexOptions.updated(modelUuid, result),
        )
      }

    case _ =>
      (flexOptions, allFlexOptions)
  }

}

object EmServiceBaseCore {

  def empty: EmServiceBaseCore = EmServiceBaseCore()
}
