/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.apache.pekko.actor.SupervisorStrategy.Stop
import org.apache.pekko.actor.{
  Actor,
  ActorRef,
  AllForOneStrategy,
  Props,
  Stash,
  SupervisorStrategy,
  Terminated
}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.messages.{SchedulerMessage, StopMessage}
import edu.ie3.simona.ontology.trigger.Trigger.{
  InitializeGridAgentTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim.{
  EmergencyShutdownInitiated,
  SimonaSimStateData
}
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

/** Main entrance point to a simona simulation as top level actor. This actors
  * allows to control the simulation in the sense that messages for
  * initialization and time steps can be send to this actor and will be handled
  * accordingly. This actor does NOT report back any simulation status except of
  * if the overall simulation has been successful or not. For specific status
  * information, the user needs to pass in and subscribe to the corresponding
  * listener e.g. [[edu.ie3.simona.event.listener.RuntimeEventListener]] for
  * simulation status or [[edu.ie3.simona.event.listener.ResultEventListener]]
  * for result events
  *
  * @version 0.1
  * @since 01.07.20
  */
class SimonaSim(simonaSetup: SimonaSetup)
    extends Actor
    with LazyLogging
    with Stash {

  override val supervisorStrategy: SupervisorStrategy =
    AllForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
      case ex: Exception =>
        self ! EmergencyShutdownInitiated
        logger.error(
          "The simulation's guardian received an uncaught exception. - {}: \"{}\" - Start emergency shutdown.",
          ex.getClass.getSimpleName,
          ex.getMessage
        )
        Stop
    }

  /* start listener */
  // output listener
  val systemParticipantsListener: Seq[ActorRef] =
    simonaSetup.systemParticipantsListener(context)

  // runtime event listener
  val runtimeEventListener: ActorRef[RuntimeEvent] =
    simonaSetup.runtimeEventListener(context)

  /* start scheduler */
  val timeAdvancer: ActorRef[SchedulerMessage] =
    simonaSetup.timeAdvancer(context, self, runtimeEventListener)
  val scheduler: ActorRef = simonaSetup.scheduler(context, timeAdvancer)

  /* start services */
  // primary service proxy
  val (
    primaryServiceProxy: ActorRef,
    primaryProxyInitData: InitPrimaryServiceProxyStateData
  ) = simonaSetup.primaryServiceProxy(context, scheduler)

  // weather service
  val (weatherService: ActorRef, weatherInitData: InitWeatherServiceStateData) =
    simonaSetup.weatherService(context, scheduler)

  val extSimulationData: ExtSimSetupData =
    simonaSetup.extSimulations(context, scheduler)

  // init all services
  scheduler ! ScheduleTriggerMessage(
    InitializeServiceTrigger(primaryProxyInitData),
    primaryServiceProxy
  )

  scheduler ! ScheduleTriggerMessage(
    InitializeServiceTrigger(weatherInitData),
    weatherService
  )

  // init ext simulation actors
  extSimulationData.allActorsAndInitTriggers.foreach {
    case (actor, initTrigger) =>
      scheduler ! ScheduleTriggerMessage(
        initTrigger,
        actor
      )
  }

  /* start grid agents  */
  val gridAgents: Map[ActorRef, GridAgentInitData] = simonaSetup.gridAgents(
    context,
    EnvironmentRefs(
      scheduler,
      primaryServiceProxy,
      weatherService,
      extSimulationData.evDataService
    ),
    systemParticipantsListener
  )

  /* watch all actors */
  systemParticipantsListener.foreach(context.watch)
  context.watch(runtimeEventListener.toClassic)
  context.watch(timeAdvancer.toClassic)
  context.watch(scheduler)
  context.watch(primaryServiceProxy)
  context.watch(weatherService)
  gridAgents.keySet.foreach(context.watch)

  override def receive: Receive = simonaSimReceive(SimonaSimStateData())

  def simonaSimReceive(data: SimonaSim.SimonaSimStateData): Receive = {

    case InitSimMessage =>
      // initialize grid agents
      gridAgents.foreach { case (gridAgent, gridAgentInitData) =>
        scheduler ! ScheduleTriggerMessage(
          InitializeGridAgentTrigger(gridAgentInitData),
          gridAgent
        )
      }

      // tell scheduler to process all init messages
      timeAdvancer ! StartScheduleMessage()
      context become simonaSimReceive(data.copy(initSimSender = sender()))

    case StartScheduleMessage(pauseScheduleAtTick) =>
      timeAdvancer ! StartScheduleMessage(pauseScheduleAtTick)

    case msg @ (SimulationSuccessfulMessage | SimulationFailureMessage) =>
      val simulationSuccessful = msg match {
        case SimulationSuccessfulMessage =>
          logger.info(
            "Simulation terminated successfully. Stopping children ..."
          )
          true
        case SimulationFailureMessage =>
          logger.error(
            "An error occurred during the simulation. See stacktrace for details."
          )
          false
      }

      // stop all children
      stopAllChildrenGracefully(simulationSuccessful)

      // wait for listeners and send final message to parent
      context become waitingForListener(
        data.initSimSender,
        simulationSuccessful,
        systemParticipantsListener
      )

    case EmergencyShutdownInitiated =>
      logger.debug(
        "Simulation guardian is aware, that emergency shutdown has been initiated. Inform the init sender."
      )
      data.initSimSender ! SimulationFailureMessage
      context.become(emergencyShutdownReceive)

    case Terminated(actorRef) =>
      logger.error(
        "An actor ({}) unexpectedly terminated. Shut down all children gracefully and report simulation " +
          "failure. See logs and possible stacktrace for details.",
        actorRef
      )

      // stop all children
      stopAllChildrenGracefully(simulationSuccessful = false)

      // wait for listeners and send final message to parent
      context become waitingForListener(
        data.initSimSender,
        successful = false,
        systemParticipantsListener
      )
  }

  def emergencyShutdownReceive: Receive = {
    case EmergencyShutdownInitiated =>
    /* Nothing to do. Already know about emergency shutdown. */

    case Terminated(actorRef) =>
      logger.debug("'{}' successfully terminated.", actorRef)

    case unsupported =>
      logger.warn(
        "Received the following message. Simulation is in emergency shutdown mode. Will neglect that " +
          "message!\n\t{}",
        unsupported
      )
  }

  def waitingForListener(
      initSimSender: ActorRef,
      successful: Boolean,
      remainingListeners: Seq[ActorRef]
  ): Receive = {
    case Terminated(actor) if remainingListeners.contains(actor) =>
      val updatedRemainingListeners = remainingListeners.filterNot(_ == actor)

      logger.debug(
        "Listener {} has been terminated. Remaining listeners: {}",
        actor,
        updatedRemainingListeners
      )

      if (updatedRemainingListeners.isEmpty) {
        logger.info(
          "The last remaining result listener has been terminated. Exiting."
        )

        val msg =
          if (successful) SimulationSuccessfulMessage
          else SimulationFailureMessage
        // inform initSimMessage Sender
        initSimSender ! msg
      }

      context become waitingForListener(
        initSimSender,
        successful,
        updatedRemainingListeners
      )
  }

  def stopAllChildrenGracefully(
      simulationSuccessful: Boolean
  ): Unit = {
    gridAgents.foreach { case (gridAgentRef, _) =>
      context.unwatch(gridAgentRef)
      gridAgentRef ! StopMessage(simulationSuccessful)
    }

    context.unwatch(scheduler)
    context.stop(scheduler)

    context.unwatch(weatherService)
    context.stop(weatherService)

    extSimulationData.extSimAdapters.foreach { case (ref, _) =>
      context.unwatch(ref)
      ref ! StopMessage(simulationSuccessful)
    }
    extSimulationData.extDataServices.foreach { case (ref, _) =>
      context.unwatch(ref)
      context.stop(ref)
    }

    // do not unwatch the result listeners, as we're waiting for their termination
    systemParticipantsListener.foreach(
      _ ! StopMessage(simulationSuccessful)
    )

    context.unwatch(runtimeEventListener.toClassic)
    context.stop(runtimeEventListener.toClassic)

    logger.debug("Stopping all listeners requested.")
  }
}

object SimonaSim {

  /** Object to indicate to the scheduler itself, that an uncaught exception has
    * triggered an emergency shutdown of the simulation system
    */
  case object EmergencyShutdownInitiated

  private[SimonaSim] final case class SimonaSimStateData(
      initSimSender: ActorRef = ActorRef.noSender
  )

  def props(simonaSetup: SimonaSetup): Props =
    Props(new SimonaSim(simonaSetup))

}
