/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.cluster

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.cluster.sharding.ShardRegion.ShardId
import akka.cluster.sharding.{
  ClusterSharding,
  ClusterShardingSettings,
  ShardRegion
}
import edu.ie3.simona.actor.SimonaActorNaming.typeName
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.fixedfeedin.FixedFeedInAgent
import edu.ie3.simona.agent.participant.load.LoadAgent
import edu.ie3.simona.agent.participant.pv.PVAgent
import edu.ie3.simona.agent.participant.wec.WecAgent
import edu.ie3.simona.akka.SimonaActorRef.RichActorRefFactory
import edu.ie3.simona.model.participant.load.LoadModelBehaviour
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup

object SimonaSharding {

  private def gridShards(
      system: ActorSystem,
      simonaSetup: SimonaStandaloneSetup
  ): Iterable[Props] = {
    val simonaSim = system.createSingletonOf(
      SimonaSim.props(simonaSetup)
    )
    val systemParticipantListener =
      simonaSetup.systemParticipantsListener(system, simonaSim)
    val runtimeEventListener = simonaSetup.runtimeEventListener(system)
    val scheduler = simonaSetup.scheduler(system, runtimeEventListener)
    val (weatherService, _) = simonaSetup.weatherService(system, scheduler)
    val (primaryServiceProxy, _) =
      simonaSetup.primaryServiceProxy(system, scheduler)
    val evMovementsService =
      simonaSetup.extSimulations(system, scheduler).evDataService

    val environmentRefs = EnvironmentRefs(
      scheduler,
      primaryServiceProxy,
      weatherService,
      evMovementsService
    )

    Seq(
      GridAgent.props(
        environmentRefs,
        simonaSetup.simonaConfig,
        systemParticipantListener
      ),
      LoadAgent.props(
        scheduler,
        systemParticipantListener,
        LoadModelBehaviour.FIX
      ),
      LoadAgent.props(
        scheduler,
        systemParticipantListener,
        LoadModelBehaviour.PROFILE
      ),
      LoadAgent.props(
        scheduler,
        systemParticipantListener,
        LoadModelBehaviour.RANDOM
      ),
      FixedFeedInAgent.props(
        scheduler,
        systemParticipantListener
      ),
      PVAgent.props(
        scheduler,
        systemParticipantListener
      ),
      WecAgent.props(
        scheduler,
        systemParticipantListener
      )
    )
  }

  def startGridSharding(
      system: ActorSystem,
      simonaSetup: SimonaStandaloneSetup
  ): Unit = {
    val noOfShards =
      system.settings.config.getInt("akka.cluster.sharding.number-of-shards")
    gridShards(system, simonaSetup).foreach { props =>
      startSharding(
        typeName(props),
        props,
        system,
        noOfShards
      )
    }
  }

  private def startSharding(
      typeName: String,
      props: Props,
      system: ActorSystem,
      numberOfShards: Int
  ): ActorRef =
    ClusterSharding(system).start(
      typeName = typeName,
      entityProps = props,
      settings = ClusterShardingSettings(system),
      extractShardId = extractShardId(numberOfShards),
      extractEntityId = extractEntityId
    )

  private val extractEntityId: ShardRegion.ExtractEntityId = {
    case ShardMessage(_, entityId, msg) =>
      (entityId, msg)
    case x =>
      throw new RuntimeException(
        s"Unexpected message $x for entityId extraction!"
      )
  }

  private val extractShardId: Int => ShardRegion.ExtractShardId =
    (numberOfShards: Int) => {
      case ShardMessage(shardId, _, _) =>
        computeShardId(shardId, numberOfShards)
      case x =>
        throw new RuntimeException(
          s"Unexpected message $x for shardId extraction!"
        )
    }

  private def computeShardId(shard: String, numberOfShards: Int): ShardId = {
    (math.abs(shard.hashCode()) % numberOfShards).toString
  }
}
