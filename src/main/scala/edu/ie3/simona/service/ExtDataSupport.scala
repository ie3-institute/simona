/*
 * © 2021-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.api.ontology.DataMessageFromExt
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  ScheduleServiceActivation,
  ServiceResponseMessage,
}
import edu.ie3.simona.ontology.messages.{
  Activation,
  SchedulerMessage,
  ServiceMessage,
}
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

/** Trait that enables handling of external data.
  */
trait ExtDataSupport {
  this: SimonaService =>

  override type Message >: ServiceMessage | Activation |
    ServiceResponseMessage | DataMessageFromExt

  override protected def idleExternal(using
      stateData: S,
      scheduler: ActorRef[SchedulerMessage],
  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    case (ctx, ScheduleServiceActivation(tick, unlockKey)) =>
      scheduler ! ScheduleActivation(
        ctx.self,
        tick,
        Some(unlockKey),
      )

      idle

    case (_, extMsg: DataMessageFromExt) =>
      val updatedStateData = handleDataMessage(extMsg)

      idle(using updatedStateData, scheduler)

    case (ctx, extResponseMsg: ServiceResponseMessage) =>
      val updatedStateData = handleDataResponseMessage(extResponseMsg, ctx)

      idle(using updatedStateData, scheduler)
  }

  /** Handle a message from outside the simulation
    *
    * @param extMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  protected def handleDataMessage(
      extMsg: DataMessageFromExt
  )(using serviceStateData: S): S

  /** Handle a message from inside SIMONA sent to external
    *
    * @param extResponseMsg
    *   the external incoming message
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the updated state data
    */
  protected def handleDataResponseMessage(
      extResponseMsg: ServiceResponseMessage,
      ctx: ActorContext[Message],
  )(using serviceStateData: S): S
}
