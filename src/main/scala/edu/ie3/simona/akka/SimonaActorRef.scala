/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.akka

import akka.actor.{
  Actor,
  ActorContext,
  ActorRef,
  ActorRefFactory,
  ActorSystem,
  InvalidActorNameException,
  PoisonPill,
  Props
}
import akka.cluster.sharding.ClusterSharding
import akka.cluster.sharding.ShardRegion.{EntityId, ShardId}
import akka.cluster.singleton.{
  ClusterSingletonManager,
  ClusterSingletonManagerSettings,
  ClusterSingletonProxy,
  ClusterSingletonProxySettings
}
import akka.pattern.ask
import akka.util.Timeout
import edu.ie3.simona.actor.SimonaActorNaming._
import edu.ie3.simona.cluster.ShardMessage

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

trait SimonaActorRef {

  /** @param system
    *   We can't use ActorContext instead here, since this method needs to work
    *   outside of actors as well
    */
  def !(message: Any)(implicit
      sender: ActorRef = Actor.noSender,
      system: ActorSystem
  ): Unit

  def ?(
      message: Any
  )(implicit timeout: Timeout, system: ActorSystem): Future[Any]

  private[akka] def watchedBy(context: ActorContext): Unit

  private[akka] def unwatchedBy(context: ActorContext): Unit

  private[akka] def stoppedBy(context: ActorContext): Unit
}

object SimonaActorRef {

  /** A regular [[ActorRef]] that is usually used within single-machine
    * computation. Messages are just simply forwarded to the given [[ActorRef]]
    *
    * @param actorRef
    *   the ActorRef
    */
  final case class LocalActorRef(actorRef: ActorRef) extends SimonaActorRef {
    override def !(
        message: Any
    )(implicit
        sender: ActorRef = Actor.noSender,
        system: ActorSystem
    ): Unit = {
      actorRef.!(message)(sender)
    }

    override def ?(
        message: Any
    )(implicit timeout: Timeout, system: ActorSystem): Future[Any] = {
      actorRef.?(message)(timeout)
    }

    override private[akka] def watchedBy(context: ActorContext): Unit =
      context.watch(actorRef)

    override private[akka] def unwatchedBy(context: ActorContext): Unit =
      context.unwatch(actorRef)

    override private[akka] def stoppedBy(context: ActorContext): Unit =
      context.stop(actorRef)

  }

  /** We exploit the sharding mechanics which holds entities together in shards.
    * Normally, the shardId is derived from the entityId, which means that
    * entities are distributed among shards without meaningful order. Here,
    * shardId is specified explicitly, in order to force deployment of system
    * participants in groups according to their subgrid
    *
    * @param typeName
    *   the type name of the agent. Each agent that is started with unique
    *   [[Props]] needs a distinct type name
    * @param shardId
    *   the shard id that the entity (agent) should run in
    * @param entityId
    *   the entity id of the entity
    */
  final case class ClusterEntityRef private (
      typeName: String,
      shardId: ShardId,
      entityId: EntityId
  ) extends SimonaActorRef {

    override def !(
        message: Any
    )(implicit
        sender: ActorRef = Actor.noSender,
        system: ActorSystem
    ): Unit = {
      // shard region
      val sharding = ClusterSharding(system)
      val shardRegion = sharding.shardRegion(typeName)
      shardRegion ! ShardMessage(shardId, entityId, message)
    }

    override def ?(
        message: Any
    )(implicit timeout: Timeout, system: ActorSystem): Future[Any] = {
      val sharding = ClusterSharding(system)
      val shardRegion = sharding.shardRegion(typeName)
      shardRegion ? ShardMessage(shardId, entityId, message)
    }

    override private[akka] def watchedBy(context: ActorContext): Unit = {
      // It probably does not make sense to watch/unwatch a sharded entity, since it can be stopped/started by re-balancing
    }

    override private[akka] def unwatchedBy(context: ActorContext): Unit = {
      // It probably does not make sense to watch/unwatch a sharded entity, since it can be stopped/started by re-balancing
    }

    override private[akka] def stoppedBy(context: ActorContext): Unit = {
      this.!(PoisonPill)(system = context.system)
    }
  }

  object ClusterEntityRef {
    val gridShardIdPrefix = "GridShard_"
    val mainShardId = "MainShard"

    /** Creates a ClusterEntityRef for a sharded actor
      * @param typeName
      *   the type name of the actor
      * @param subnet
      *   the number of the subgrid
      * @param entityId
      *   the entity id
      * @return
      *   a ClusterEntityRef
      */
    def apply(
        typeName: String,
        subnet: Int,
        entityId: String
    ): ClusterEntityRef =
      ClusterEntityRef(typeName, gridShardIdPrefix + subnet, entityId)
  }

  /** A reference to a Singleton actor.
    *
    * @param actorRef
    *   actor ref of the singleton actor, usually a proxy
    * @param actorName
    *   name of the singleton actor, has to be unique
    */
  final case class SingletonRef(
      actorRef: ActorRef,
      actorName: String
  ) extends SimonaActorRef {

    override def !(
        message: Any
    )(implicit
        sender: ActorRef = Actor.noSender,
        system: ActorSystem
    ): Unit = {
      actorRef.!(message)(sender)
    }

    override def ?(
        message: Any
    )(implicit timeout: Timeout, system: ActorSystem): Future[Any] = {
      actorRef.?(message)(timeout)
    }

    override private[akka] def watchedBy(context: ActorContext): Unit = {
      implicit val timeout: Timeout = 5.seconds
      implicit val executor: ExecutionContext = context.dispatcher
      val managerPath = SingletonRef.userPath + actorName

      context.actorSelection(managerPath).resolveOne().flatMap {
        case managerRef: ActorRef =>
          Future {
            context.watch(managerRef)
          }
        case _ =>
          throw new RuntimeException(
            s"Actor $managerPath could not be watched because its manager could not be found."
          )
      }
    }

    override private[akka] def unwatchedBy(context: ActorContext): Unit = {
      implicit val timeout: Timeout = 5.seconds
      implicit val executor: ExecutionContext = context.dispatcher
      val managerPath = SingletonRef.userPath + actorName

      context.actorSelection(managerPath).resolveOne().flatMap {
        case managerRef: ActorRef =>
          Future {
            context.unwatch(managerRef)
          }
        case _ =>
          throw new RuntimeException(
            s"Actor $managerPath could not be watched because its manager could not be found."
          )
      }
    }

    override private[akka] def stoppedBy(context: ActorContext): Unit =
      context.stop(actorRef)

    override def equals(obj: Any): Boolean = obj match {
      case SingletonRef(_, actorName2) =>
        // since actorRefs can differ due to different proxy names, we only compare actorNames
        actorName == actorName2
      case _ =>
        false
    }

    override def hashCode(): Int =
      actorName.##
  }

  object SingletonRef {
    val userPath = "/user/"
  }

  /** Enhances [[ActorRefFactory]] by providing methods that create
    * [[SimonaActorRef]]s
    *
    * @param refFactory
    *   ActorSystem or ActorContext
    */
  implicit class RichActorRefFactory(private val refFactory: ActorRefFactory) {

    /** Creates a SimonaActorRef (either [[ClusterEntityRef]] or
      * [[LocalActorRef]]) for an actor that should only exist once per given
      * class and subnet.
      *
      * @param props
      *   used for LocalActorRef only
      * @param subnetNo
      *   used for ClusterEntityRef only
      * @return
      *   a SimonaActorRef
      */
    def createShardedEntityOf(props: Props, subnetNo: Int): SimonaActorRef =
      createShardedEntity(
        props,
        subnetNo,
        actorName(props, subnetNo.toString)
      )

    /** Creates a SimonaActorRef (either [[ClusterEntityRef]] or
      * [[LocalActorRef]]) for an actor that should only exist once per given
      * class, subnet and id, but can exist multiple times per class and subnet.
      *
      * @param props
      *   used for LocalActorRef only
      * @param subnetNo
      *   used for ClusterEntityRef only
      * @param actorId
      *   the actor id
      * @return
      *   a SimonaActorRef
      */
    def createShardedEntityWithIdOf(
        props: Props,
        subnetNo: Int,
        actorId: String
    ): SimonaActorRef =
      createShardedEntity(props, subnetNo, actorName(props, actorId))

    private def createShardedEntity(
        props: Props,
        subnetNo: Int,
        name: String
    ): SimonaActorRef = {
      if (refFactory.system.settings.HasCluster)
        ClusterEntityRef(typeName(props), subnetNo, name)
      else
        LocalActorRef(
          refFactory.actorOf(
            props,
            name
          )
        )
    }

    /** Creates a SimonaActorRef (either [[SingletonRef]] or [[LocalActorRef]])
      * for an actor with given props. Only one single actor can be created with
      * this function, actor name is equivalent to type name of the actor.
      *
      * @param props
      *   the props used for creating the actor
      * @return
      *   a SimonaActorRef
      */
    def createSingletonOf(props: Props): SimonaActorRef =
      createSingleton(props, typeName(props))

    /** Creates a SimonaActorRef (either [[SingletonRef]] or [[LocalActorRef]])
      * for an actor with given props and id. Singletons are identified by name,
      * so using different actor ids creates different singletons!
      *
      * @param props
      *   the props used for creating the actor
      * @param actorId
      *   has to be the same for all singleton refs to the same actor
      * @return
      *   a SimonaActorRef
      */
    def createSingletonWithIdOf(
        props: Props,
        actorId: String
    ): SimonaActorRef =
      createSingleton(props, actorName(props, actorId))

    private def createSingleton(props: Props, name: String) = {
      if (refFactory.system.settings.HasCluster)
        createSingletonActorAndRef(
          refFactory.system,
          props,
          name
        )
      else
        LocalActorRef(
          refFactory.actorOf(
            props,
            name
          )
        )
    }
  }

  /** Enhances [[ActorRef]] by providing methods that wrap itself in a
    * [[SimonaActorRef]]
    *
    * @param actorRef
    *   the ActorRef that should be wrapped
    */
  implicit class RichActorRef(private val actorRef: ActorRef) {

    /** Wraps the ActorRef in a LocalActorRef. Mostly useful for debugging and
      * logging purposes
      *
      * @return
      *   the given ActorRef wrapped in a LocalActorRef
      */
    def asLocal: LocalActorRef = LocalActorRef(actorRef)
  }

  /** Wraps the self-ActorRef in a SimonaActorRef (either [[ClusterEntityRef]]
    * or [[LocalActorRef]]).
    *
    * @param subnetNo
    *   used for ClusterEntityRef only
    * @param context
    *   the ActorContext of the actor to create the ref for
    * @return
    *   a SimonaActorRef for the SystemParticipant
    */
  def selfSharded(
      subnetNo: Int
  )(implicit context: ActorContext): SimonaActorRef = {
    if (context.system.settings.HasCluster) {
      ClusterEntityRef(
        typeName(context.self),
        subnetNo,
        actorName(context.self)
      )
    } else
      LocalActorRef(context.self)
  }

  /** Wraps the self-ActorRef in a SimonaActorRef (either [[SingletonRef]] or
    * [[LocalActorRef]]). Actor name is extracted from the ActorRef.
    *
    * @param context
    *   the ActorContext of the actor to create the ref for
    * @return
    *   a SimonaActorRef to self
    */
  def selfSingleton(implicit context: ActorContext): SimonaActorRef = {
    if (context.system.settings.HasCluster)
      SingletonRef(context.self, actorName(context.self))
    else
      LocalActorRef(context.self)
  }

  /** Adaptation of akka.cluster.typed.internal.AdaptedClusterSingletonImpl of
    * Akka Typed, which is licensed under Apache 2.0. TODO check how these
    * licensing matters work
    */
  private def createSingletonActorAndRef(
      system: ActorSystem,
      props: Props,
      actorName: String
  ): SingletonRef = {
    try {
      system.actorOf(
        ClusterSingletonManager.props(
          singletonProps = props,
          terminationMessage = PoisonPill,
          settings = ClusterSingletonManagerSettings(system)
        ),
        name = actorName
      )
    } catch {
      case ex: InvalidActorNameException
          if ex.getMessage.endsWith("is not unique!") =>
      // This is fine. We just wanted to make sure it is running and it already is
      // akka-cluster-type: AdaptedClusterSingletonImpl does this the same way
    }

    SingletonRef(
      system.actorOf(
        ClusterSingletonProxy.props(
          singletonManagerPath = SingletonRef.userPath + actorName,
          settings = ClusterSingletonProxySettings(system)
        ) // do not designate a name here, as multiple proxies can exist
      ),
      actorName
    )
  }

  private implicit class ActorRefFactoryToActorSystem[A](
      private val refFactory: ActorRefFactory
  ) {
    def system: ActorSystem =
      refFactory match {
        case system: ActorSystem   => system
        case context: ActorContext => context.system
      }
  }
}
