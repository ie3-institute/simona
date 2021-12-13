/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.cluster

import akka.cluster.sharding.ShardRegion.{EntityId, ShardId}

/** Message that is used to address an entity at a specific shard. Normally,
  * shardId is derived from entityId. We explicitly specify shardId because
  * entities are located at specific shards.
  *
  * @param shardId
  *   The shard id that the entity can be found at
  * @param entityId
  *   The entity id
  * @param msg
  *   The msg to deliver to the entity
  */
final case class ShardMessage(
    shardId: ShardId,
    entityId: EntityId,
    msg: Any
)
