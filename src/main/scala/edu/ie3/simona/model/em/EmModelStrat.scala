/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import squants.energy.Kilowatts

import java.util.UUID

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
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ],
      target: squants.Power
  ): Iterable[(UUID, squants.Power)]

  def adaptFlexOptions(
      spi: AssetInput,
      flexOptions: ProvideMinMaxFlexOptions
  ): ProvideMinMaxFlexOptions
}

object EmModelStrat {
  val tolerance: squants.Power = Kilowatts(1e-6d)
}
