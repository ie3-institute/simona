/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Power

/** Offers method for aggregating flex options from connected agents which will
  * then be provided to a superior EmAgent
  */
trait EmAggregateFlex {

  /** Aggregates flex options of connected devices to one flex options object
    * that describes the flexibility of this EmAgent
    * @param flexOptions
    *   the flex options of all connected agents
    * @return
    *   aggregated reference, minimum and maximum power
    */
  def aggregateFlexOptions(
      flexOptions: Iterable[ProvideMinMaxFlexOptions]
  ): (
      ComparableQuantity[Power],
      ComparableQuantity[Power],
      ComparableQuantity[Power]
  )
}
