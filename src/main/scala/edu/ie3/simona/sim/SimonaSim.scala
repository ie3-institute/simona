/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import akka.actor.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.agent.state.AgentState.Finish
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  InitializeGridAgentTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Main entrance point to a simona simulation as top level actor. This actors
  * allows to control the simulation in the sense that messages for
  * initialization and time steps can be send to this actor and will be handled
  * accordingly. This actor does NOT report back any simulation status. For
  * specific status information, the user needs to pass in and subscribe to the
  * corresponding listener e.g.
  * [[edu.ie3.simona.event.listener.RuntimeEventListener]] for simulation status
  * or [[edu.ie3.simona.event.listener.ResultEventListener]] for result events
  *
  * @since 01.07.20
  */
object SimonaSim {

  sealed trait Request

  final case object InitSimMessage extends Request

  /** */
  private final case object InitActors extends Request

  final case class ServiceInitComplete(sender: ActorRef) extends Request

  final case class StartScheduleMessage(pauseScheduleAtTick: Long)
      extends Request

  final case object SimulationSuccessfulMessage extends Request

  final case object SimulationFailureMessage extends Request

  private final case object StopListeners extends Request

  private final case class StateData(
      systemParticipantsListener: Seq[ActorRef],
      runtimeEventListener: Seq[ActorRef],
      environmentRefs: EnvironmentRefs,
      gridAgents: Seq[ActorRef]
  )

  private final case class InitStateData(
      stateData: StateData,
      gridAgentsInit: Map[ActorRef, GridAgentInitData],
      actorInitWaitingList: Seq[ActorRef]
  )

  def apply(simonaSetup: SimonaSetup): Behavior[Request] = create(
    simonaSetup: SimonaSetup
  )

  private def create(simonaSetup: SimonaSetup): Behavior[Request] =
    Behaviors.receivePartial { case (context, InitSimMessage) =>
      /* start listener */
      // output listener
      val systemParticipantsListener =
        simonaSetup.systemParticipantsListener(context.toClassic, context.self)

      // runtime event listener
      val runtimeEventListener =
        simonaSetup.runtimeEventListener(context.toClassic)

      /* start scheduler */
      val scheduler =
        simonaSetup.scheduler(context.toClassic, runtimeEventListener)

      /* start services */
      // primary service proxy
      val (primaryServiceProxy, primaryProxyInitData) =
        simonaSetup.primaryServiceProxy(context.toClassic, scheduler)

      // weather service
      val (weatherService, weatherInitData) =
        simonaSetup.weatherService(context.toClassic, scheduler)

      val extSimulationData =
        simonaSetup.extSimulations(context.toClassic, scheduler)

      // init all services
      scheduler ! SchedulerMessage.ScheduleTriggerMessage(
        InitializeServiceTrigger(primaryProxyInitData),
        primaryServiceProxy
      )

      scheduler ! SchedulerMessage.ScheduleTriggerMessage(
        InitializeServiceTrigger(weatherInitData),
        weatherService
      )

      // init ext simulation actors
      extSimulationData.allActorsAndInitTriggers.foreach {
        case (actor, initTrigger) =>
          scheduler ! SchedulerMessage.ScheduleTriggerMessage(
            initTrigger,
            actor
          )
      }

      val environmentRefs = EnvironmentRefs(
        scheduler,
        primaryServiceProxy,
        weatherService,
        extSimulationData.evDataService
      )

      /* start grid agents  */
      val gridAgents = simonaSetup.gridAgents(
        context.toClassic,
        environmentRefs,
        systemParticipantsListener
      )

      /* watch all actors */
      systemParticipantsListener.foreach(context.watch(_))
      runtimeEventListener.foreach(context.watch(_))
      context.watch(scheduler)
      context.watch(primaryServiceProxy)
      context.watch(weatherService)
      gridAgents.keySet.foreach(context.watch(_))

      /* watch for the following services until their initialization is finished */
      val actorInitWaitingList = systemParticipantsListener

      val initStateData = InitStateData(
        StateData(
          systemParticipantsListener,
          runtimeEventListener,
          environmentRefs,
          gridAgents.keySet.toSeq
        ),
        gridAgents,
        actorInitWaitingList
      )

      if (actorInitWaitingList.isEmpty) {
        context.self ! InitActors
        init(initStateData)
      } else initWaiting(initStateData)
    }

  private def initWaiting(initStateData: InitStateData): Behavior[Request] =
    Behaviors.withStash(10) { buffer =>
      Behaviors.receivePartial {

        case (_, ServiceInitComplete(sender)) =>
          val updatedStateData = initStateData.copy(
            actorInitWaitingList =
              initStateData.actorInitWaitingList.filterNot(_.equals(sender))
          )

          if (updatedStateData.actorInitWaitingList.isEmpty) {
            buffer.stash(InitActors)
            buffer.unstashAll(init(initStateData))
          } else
            initWaiting(updatedStateData)

        case (_, msg) =>
          buffer.stash(msg)
          Behaviors.same

      }
    }

  private def init(initStateData: InitStateData): Behavior[Request] =
    Behaviors.receivePartial[Request] { case (context, InitActors) =>
      // initialize grid agents
      initStateData.gridAgentsInit.foreach {
        case (gridAgent, gridAgentInitData) =>
          initStateData.stateData.environmentRefs.scheduler ! SchedulerMessage
            .ScheduleTriggerMessage(
              InitializeGridAgentTrigger(gridAgentInitData),
              gridAgent
            )
      }

      // tell scheduler to process all init messages
      initStateData.stateData.environmentRefs.scheduler ! SchedulerMessage
        .InitSimMessage(context.self.toClassic)

      idle(initStateData.stateData)
    }

  private def idle(stateData: StateData): Behavior[Request] =
    Behaviors.withTimers { timers =>
      Behaviors.receivePartial[Request] {

        case (_, StartScheduleMessage(pauseScheduleAtTick)) =>
          stateData.environmentRefs.scheduler ! StartScheduleMessage(
            pauseScheduleAtTick
          )
          Behaviors.same

        case (
              context,
              msg @ (SimulationSuccessfulMessage | SimulationFailureMessage)
            ) =>
          msg match {
            case SimulationSuccessfulMessage =>
              context.log.info(
                "Simulation terminated successfully. Stopping children ..."
              )
            case SimulationFailureMessage =>
              context.log.error(
                "An error occurred during the simulation. See stacktrace for details."
              )
          }

          // stop all children
          val listenerDelay: FiniteDuration = 500.millis
          stateData.gridAgents.foreach(context.unwatch(_))
          stateData.gridAgents.foreach(_ ! Finish)

          context.unwatch(stateData.environmentRefs.scheduler)
          context.stop(stateData.environmentRefs.scheduler)

          context.unwatch(stateData.environmentRefs.weather)
          context.stop(stateData.environmentRefs.weather)

          /* Stop listeners with a delay */
          context.log.debug(
            "Waiting for {} to stop the listeners.",
            listenerDelay
          )

          timers.startSingleTimer(
            StopListeners,
            listenerDelay
          )

          Behaviors.same

        case (context, StopListeners) =>
          stateData.systemParticipantsListener.foreach(context.unwatch(_))
          stateData.systemParticipantsListener.foreach(context.stop(_))

          stateData.runtimeEventListener.foreach(context.unwatch(_))
          stateData.runtimeEventListener.foreach(context.stop(_))

          Behaviors.stopped

      }
    }

}
