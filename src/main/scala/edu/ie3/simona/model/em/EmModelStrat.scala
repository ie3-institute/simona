/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import squants.Power
import squants.energy.Kilowatts

import java.util.UUID

/** Trait that can be enhanced by multiple strategies to disaggregate
  * flexibility control, i.e. given a target power, determining flex control for
  * connected agents
  */
trait EmModelStrat {

  /** Determine the target power (set points) of connected agents that provided
    * flex options before. Connected agents that have no result assigned in
    * return data are
    *
    * @param flexOptions
    *   The flex options per connected agent
    * @param target
    *   The target power to aim for when utilizing flexibility
    * @return
    *   Power set points for connected agents, if applicable
    */
  def determineFlexControl(
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ],
      target: Power,
  ): Iterable[(UUID, Power)]

  /** Depending on the model strategy used, not all flex options provided by
    * connected agents might be usable by the parent
    * [[edu.ie3.simona.agent.em.EmAgent]]. This method adapts the given flex
    * options based on the given [[AssetInput]].
    *
    * @param assetInput
    *   The [[AssetInput]] of the connected agent providing the flex options
    * @param flexOptions
    *   The flex options
    * @return
    *   adapted flex options
    */
  def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: ProvideMinMaxFlexOptions,
  ): ProvideMinMaxFlexOptions
}

object EmModelStrat {
  val tolerance: Power = Kilowatts(1e-6d)
}
