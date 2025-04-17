/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.em

import edu.ie3.simona.api.data.em.ontology._
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegisterForEmDataService
import edu.ie3.simona.util.SimonaConstants.PRE_INIT_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger

import java.time.ZonedDateTime
import java.util.UUID

case class EmServiceBaseCore(
    override val lastFinishedTick: Long = PRE_INIT_TICK,
    override val uuidToFlexAdapter: Map[UUID, ActorRef[FlexRequest]] = Map.empty,
) extends EmServiceCore {

  override def handleRegistration(
      registrationMsg: RegisterForEmDataService
  ): EmServiceBaseCore =
    copy(uuidToFlexAdapter =
      uuidToFlexAdapter ++ Map(
        registrationMsg.modelUuid -> registrationMsg.flexAdapter
      )
    )

  override def handleExtMessage(tick: Long, extMSg: EmDataMessageFromExt)(
      implicit log: Logger
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = extMSg match {
    case provideEmSetPoints: ProvideEmSetPointData =>
      handleSetPoint(tick, provideEmSetPoints, log)

      (this, None)

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
    receiver.map { ref =>
      log.warn(s"$ref: $flexResponse")
      ref ! flexResponse
    }

    (this, None)
  }

  override def handleFlexRequest(
      flexRequest: FlexRequest,
      receiver: ActorRef[FlexRequest],
  )(implicit
      startTime: ZonedDateTime,
      log: Logger,
  ): (EmServiceCore, Option[EmDataResponseMessageToExt]) = {
    log.warn(s"$receiver: $flexRequest")
    receiver ! flexRequest

    (this, None)
  }
}

object EmServiceBaseCore {

  def empty: EmServiceBaseCore = EmServiceBaseCore()
}
