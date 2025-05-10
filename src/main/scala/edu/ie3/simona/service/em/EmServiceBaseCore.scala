/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.api.data.em.model.ExtendedFlexOptionsResult
import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegisterForEmDataService
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

final case class EmServiceBaseCore(
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToFlexAdapter: Map[UUID, ActorRef[FlexRequest]] =
      Map.empty,
    flexOptions: ReceiveDataMap[UUID, ExtendedFlexOptionsResult] =
      ReceiveDataMap.empty,
    additionalFlexOptions: Map[UUID, ExtendedFlexOptionsResult] = Map.empty,
    override val completions: ReceiveDataMap[UUID, FlexCompletion] =
      ReceiveDataMap.empty,
    structure: Map[UUID, Set[UUID]] = Map.empty,
    disaggregatedFlex: Boolean = false,
    sendOptionsToExt: Boolean = false,
    canHandleSetPoints: Boolean = false,
    setPointOption: Option[ProvideEmSetPointData] = None,
) extends EmServiceCore {

  override def handleRegistration(
      registrationMsg: RegisterForEmDataService
  ): EmServiceBaseCore = {
    val modelUuid = registrationMsg.modelUuid
    val parentUuid = registrationMsg.parentUuid

    val updatedStructure = parentUuid match {
      case Some(parent) =>
        structure.get(parent) match {
          case Some(subEm) =>
            val allSubEms = subEm + modelUuid

            structure ++ Map(parent -> allSubEms)
          case None =>
            structure ++ Map(parent -> Set(modelUuid))
        }

      case None if !structure.contains(modelUuid) =>
        structure ++ Map(modelUuid -> Set.empty[UUID])

      case _ =>
        // we already added the model as parent
        // therefore, no changes are needed
        structure
    }

    copy(
      uuidToFlexAdapter =
        uuidToFlexAdapter ++ Map(modelUuid -> registrationMsg.flexAdapter),
      completions = completions.addExpectedKeys(Set(modelUuid)),
      structure = updatedStructure,
    )
  }

  override def handleExtMessage(tick: Long, extMSg: EmDataMessageFromExt)(
      implicit log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = extMSg match {
    case requestEmFlexResults: RequestEmFlexResults =>
      val tick = requestEmFlexResults.tick
      val emEntities = requestEmFlexResults.emEntities.asScala
      val disaggregated = requestEmFlexResults.disaggregated

      if (disaggregated) {
        log.warn(s"Disaggregated flex options are currently not supported!")
      }

      emEntities.map(uuidToFlexAdapter).foreach { ref =>
        ref ! FlexActivation(tick)
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
      if (canHandleSetPoints) {
        handleSetPoint(tick, provideEmSetPoints, log)

        (this, None)
      } else {
        val tick = provideEmSetPoints.tick
        val emEntities = provideEmSetPoints.emData.keySet.asScala

        emEntities.map(uuidToFlexAdapter).foreach { ref =>
          ref ! FlexActivation(tick)
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
        s"The EmServiceBaseCore is not able to handle the message: $extMSg"
      )
  }

  override def handleFlexResponse(
      tick: Long,
      flexResponse: FlexResponse,
      receiver: Either[UUID, ActorRef[FlexResponse]],
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    receiver.foreach(_ ! flexResponse)

    flexResponse match {
      case provideFlexOptions: ProvideFlexOptions =>
        val (updated, updatedAdditional) = provideFlexOptions match {
          case ProvideFlexOptions(
                modelUuid,
                MinMaxFlexOptions(ref, min, max),
              ) =>
            val result = new ExtendedFlexOptionsResult(
              tick.toDateTime(startTime),
              modelUuid,
              modelUuid,
              min.toQuantity,
              ref.toQuantity,
              max.toQuantity,
            )

            if (flexOptions.getExpectedKeys.contains(modelUuid)) {
              (
                flexOptions.addData(modelUuid, result),
                additionalFlexOptions,
              )
            } else {
              (
                flexOptions,
                additionalFlexOptions.updated(modelUuid, result),
              )
            }

          case _ =>
            (flexOptions, additionalFlexOptions)
        }

        if (updated.isComplete) {
          // we received all flex options

          val data = updated.receivedData

          if (disaggregatedFlex) {
            // we add the disaggregated flex options

            data.foreach { case (key, value) =>
              structure(key).foreach { inferior =>
                value.addDisaggregated(inferior, updatedAdditional(inferior))
              }
            }
          }

          val updatedCore = copy(
            flexOptions = ReceiveDataMap.empty,
            canHandleSetPoints = true,
          )

          if (sendOptionsToExt) {
            // we have received an option request, that will now be answered
            (updatedCore, Some(new FlexOptionsResponse(data.asJava)))

          } else {
            setPointOption match {
              case Some(setPoints) =>
                // we have received new set points, that are not handled yet => we will handle them now
                handleSetPoint(tick, setPoints, log)

                (updatedCore, None)
              case None =>
                // we are now able to handle set points, but we have not yet received any
                (updatedCore, None)
            }
          }

        } else {
          (
            copy(
              flexOptions = updated,
              additionalFlexOptions = updatedAdditional,
            ),
            None,
          )
        }

      case completion: FlexCompletion =>
        val updated = completions.addData(completion.modelUuid, completion)

        if (updated.isComplete) {
          val allKeys = updated.receivedData.keySet

          val extMsgOption = if (tick != INIT_SIM_TICK) {
            // send completion message to external simulation, if we aren't in the INIT_SIM_TICK
            Some(new EmCompletion())
          } else None

          // every em agent has sent a completion message
          (
            copy(
              lastFinishedTick = tick,
              completions = ReceiveDataMap(allKeys),
              disaggregatedFlex = false,
              sendOptionsToExt = false,
              canHandleSetPoints = false,
            ),
            extMsgOption,
          )

        } else (copy(completions = updated), None)

      case _ =>
        (this, None)
    }
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    log.debug(s"$receiver: $flexRequest")
    receiver ! flexRequest

    (this, None)
  }
}

object EmServiceBaseCore {

  def empty: EmServiceBaseCore = EmServiceBaseCore()
}
