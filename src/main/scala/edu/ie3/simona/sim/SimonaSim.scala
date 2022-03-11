/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{
  Actor,
  ActorRef,
  AllForOneStrategy,
  PoisonPill,
  Props,
  Stash,
  SupervisorStrategy,
  Terminated
}
import akka.pattern.after
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.AgentState.Finish
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.ontology.messages.SchedulerMessage._
import edu.ie3.simona.ontology.trigger.Trigger.{
  InitializeGridAgentTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim.{
  EmergencyShutdownInitiated,
  ServiceInitComplete,
  ServiceInitResponse,
  SimonaSimStateData
}
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
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
    simonaSetup.systemParticipantsListener(context, self)

  // runtime event listener
  val runtimeEventListener: Seq[ActorRef] =
    simonaSetup.runtimeEventListener(context)

  /* start scheduler */
  val scheduler: ActorRef = simonaSetup.scheduler(context, runtimeEventListener)

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
  runtimeEventListener.foreach(context.watch)
  context.watch(scheduler)
  context.watch(primaryServiceProxy)
  context.watch(weatherService)
  gridAgents.keySet.foreach(context.watch)

  /* watch for the following services until their initialization is finished */
  private val actorInitWaitingList = systemParticipantsListener

  override def receive: Receive = {
    // short circuit if we do not wait for any inits
    if (actorInitWaitingList.isEmpty) simonaSimReceive(SimonaSimStateData())
    else setup(SimonaSimStateData(), actorInitWaitingList)
  }

  def setup(
      data: SimonaSim.SimonaSimStateData,
      actorInitWaitingList: Seq[ActorRef]
  ): Receive = {
    case ServiceInitComplete =>
      val updatedWaitingList =
        actorInitWaitingList.filterNot(_.equals(sender()))
      if (updatedWaitingList.isEmpty) {
        unstashAll()
        context become simonaSimReceive(data)
      } else
        setup(data, updatedWaitingList)

    case _ =>
      stash() // stash everything else away
  }

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
      scheduler ! InitSimMessage
      context become simonaSimReceive(data.copy(initSimSender = sender()))

    case StartScheduleMessage(pauseScheduleAtTick) =>
      scheduler ! StartScheduleMessage(pauseScheduleAtTick)

    case msg @ (SimulationSuccessfulMessage | SimulationFailureMessage) =>
      msg match {
        case SimulationSuccessfulMessage =>
          logger.info(
            "Simulation terminated successfully. Stopping children ..."
          )
        case SimulationFailureMessage =>
          logger.error(
            "An error occurred during the simulation. See stacktrace for details."
          )
      }

      // stop all children
      stopAllChildrenGracefully()

      // inform initSimMessage Sender
      data.initSimSender ! msg

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
      stopAllChildrenGracefully()

      // inform initSimMessage Sender
      data.initSimSender ! SimulationFailureMessage
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

  def stopAllChildrenGracefully(
      listenerDelay: FiniteDuration = 500.millis
  ): Unit = {
    gridAgents.foreach { case (gridAgentRef, _) =>
      context.unwatch(gridAgentRef)
    }
    gridAgents.foreach(_._1 ! Finish)

    context.unwatch(scheduler)
    context.stop(scheduler)

    context.unwatch(weatherService)
    context.stop(weatherService)

    /* Stop listeners with a delay */
    logger.debug("Waiting for {} to stop the listeners.", listenerDelay)
    Await.ready(
      after(listenerDelay, using = context.system.scheduler)(Future {
        systemParticipantsListener.foreach(context.unwatch)
        systemParticipantsListener.foreach(context.stop)

        runtimeEventListener.foreach(context.unwatch)
        runtimeEventListener.foreach(context.stop)
      }),
      listenerDelay + 500.millis
    )
    logger.debug("Stopping all listeners requested.")
  }
}

object SimonaSim {

  /** Object to indicate to the scheduler itself, that an uncaught exception has
    * triggered an emergency shutdown of the simulation system
    */
  case object EmergencyShutdownInitiated

  sealed trait ServiceInitResponse

  /** Message to be used by a service to indicate that its initialization is
    * complete
    */
  final case object ServiceInitComplete extends ServiceInitResponse

  final case class ServiceInitFailed(ex: Throwable) extends ServiceInitResponse

  private[SimonaSim] final case class SimonaSimStateData(
      initSimSender: ActorRef = ActorRef.noSender
  )

  def props(simonaSetup: SimonaSetup): Props =
    Props(new SimonaSim(simonaSetup))

}
