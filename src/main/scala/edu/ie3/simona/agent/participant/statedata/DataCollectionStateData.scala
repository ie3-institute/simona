/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.statedata

import org.apache.pekko.actor.ActorRef
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.util.scala.quantities.ApparentPower

import scala.reflect.{ClassTag, classTag}

/** State data that are used to wait and collect data provision (either primary
  * or secondary).
  *
  * @param baseStateData
  *   The basic state data
  * @param data
  *   A mapping from senders' [[ActorRef]] s to [[Option]] s on the provided
  *   data (None if not yet received)
  * @param yetTriggered
  *   True, if an [[edu.ie3.simona.ontology.messages.Activation]] has already
  *   arrived
  * @tparam PD
  *   Type of the [[PrimaryDataWithApparentPower]], that the model will produce
  *   or receive as primary data
  */
final case class DataCollectionStateData[+PD <: PrimaryDataWithApparentPower](
    baseStateData: BaseStateData[PD],
    data: Map[ActorRef, Option[_ <: Data]],
    yetTriggered: Boolean,
) extends ParticipantStateData[ApparentPower, PD] {

  /** Extract the given type of [[Data]] from the list of secondary data
    *
    * @tparam T
    *   Type of the data to look for
    * @return
    *   The secondary data
    */
  def extract[T <: Data: ClassTag](): Option[T] = {
    data.valuesIterator
      .flatMap {
        case Some(found: T) if classTag[T].runtimeClass.isInstance(found) =>
          Some(found)
        case _ => None
      }
      .toList
      .headOption
  }
}
