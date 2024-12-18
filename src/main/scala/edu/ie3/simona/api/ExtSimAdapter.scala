/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{Actor, ActorRef, PoisonPill, Props}
import edu.ie3.simona.api.ExtSimAdapter.{Create, ExtSimAdapterStateData, Stop}
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.api.simulation.ontology.{
  ActivationMessage,
  TerminationCompleted,
  TerminationMessage,
  CompletionMessage => ExtCompletionMessage,
}
import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

import scala.jdk.OptionConverters._

object ExtSimAdapter {

  def props(scheduler: ActorRef): Props =
    Props(
      new ExtSimAdapter(scheduler)
    )

  /** The [[ExtSimAdapterData]] can only be constructed once the ExtSimAdapter
    * actor is created. Thus, we need an extra initialization message.
    *
    * @param extSimData
    *   The [[ExtSimAdapterData]] of the corresponding external simulation
    */
  final case class Create(extSimData: ExtSimAdapterData, unlockKey: ScheduleKey)

  final case class Stop(simulationSuccessful: Boolean)

  final case class ExtSimAdapterStateData(
      extSimData: ExtSimAdapterData,
      currentTick: Option[Long] = None,
  )
}

final case class ExtSimAdapter(scheduler: ActorRef)
    extends Actor
    with SimonaActorLogging {
  override def receive: Receive = { case Create(extSimAdapterData, unlockKey) =>
    // triggering first time at init tick
    scheduler ! ScheduleActivation(
      self.toTyped,
      INIT_SIM_TICK,
      Some(unlockKey),
    )
    context become receiveIdle(
      ExtSimAdapterStateData(extSimAdapterData)
    )
  }

  private def receiveIdle(implicit
      stateData: ExtSimAdapterStateData
  ): Receive = {
    case Activation(tick) =>
      stateData.extSimData.queueExtMsg(
        new ActivationMessage(tick)
      )
      log.debug(
        "Tick {} has been activated in external simulation",
        tick,
      )

      context become receiveIdle(
        stateData.copy(currentTick = Some(tick))
      )

    case extCompl: ExtCompletionMessage =>
      // when multiple triggers have been sent, a completion message
      // always refers to the oldest tick

      val newTick = extCompl.nextActivation().toScala.map(Long2long)

      scheduler ! Completion(self.toTyped, newTick)
      log.debug(
        "Tick {} has been completed in external simulation",
        stateData.currentTick,
      )

      context become receiveIdle(stateData.copy(currentTick = None))

    case scheduleDataService: ScheduleDataServiceMessage =>
      val tick = stateData.currentTick.getOrElse(
        throw new RuntimeException("No tick has been triggered")
      )
      val key = ScheduleLock.singleKey(context, scheduler.toTyped, tick)

      scheduleDataService.getDataService ! ScheduleServiceActivation(
        tick,
        key,
      )

    case Stop(simulationSuccessful) =>
      // let external sim know that we have terminated
      stateData.extSimData.queueExtMsg(
        new TerminationMessage(simulationSuccessful)
      )

    case _: TerminationCompleted =>
      // external simulation has terminated as well, we can exit
      self ! PoisonPill
  }

}
