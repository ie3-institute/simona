/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Power

trait EmModelStrat {

  /** Determine the power of controllable devices such as storages
    *
    * @param flexOptions
    *   The flex options per connected system participant
    * @param target
    *   The target power to aim for when utilizing flexibility
    * @return
    *   Power set points for devices, if applicable
    */
  def determineDeviceControl(
      flexOptions: Seq[(_ <: SystemParticipantInput, ProvideMinMaxFlexOptions)],
      target: ComparableQuantity[Power]
  ): Seq[(UUID, ComparableQuantity[Power])]

  def adaptFlexOptions(
      spi: SystemParticipantInput,
      flexOptions: ProvideMinMaxFlexOptions
  ): ProvideMinMaxFlexOptions
}
