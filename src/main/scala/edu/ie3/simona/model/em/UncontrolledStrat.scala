/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.datamodel.models.input.system.{
  EvcsInput,
  HpInput,
  PvInput,
  StorageInput,
  WecInput,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat.tolerance
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.Power

import java.util.UUID

/** Determines flex control for connected agents by adhering to a priority
  * hierarchy, with some devices not controlled at all.
  *
  * @param curtailRegenerative
  *   Whether PV and WEC feed-in can be curtailed or not
  */
final case class UncontrolledStrat(curtailRegenerative: Boolean)
    extends EmModelStrat {

  /** Only heat pumps, battery storages, charging stations and PVs/WECs (if
    * enabled) are controlled by this strategy
    */
  private val controllableAssets: Seq[Class[_ <: AssetInput]] =
    Seq(classOf[HpInput], classOf[StorageInput], classOf[EvcsInput]) ++ Option
      .when(curtailRegenerative)(Seq(classOf[PvInput], classOf[WecInput]))
      .getOrElse(Seq.empty)

  /** Determine the power of controllable devices by using flexibility according
    * to a prioritized list of device types. This means that e.g. flexibility of
    * storages is used before flexibility of heat pumps is used. Priority lists
    * can differ depending on whether positive or negative flexibility needs to
    * be used.
    *
    * @param flexOptions
    *   The flex options per connected system participant
    * @param target
    *   The target power to aim for when utilizing flexibility
    * @return
    *   Power set points for devices, if applicable
    */
  override def determineFlexControl(
      flexOptions: Iterable[
        (_ <: AssetInput, ProvideMinMaxFlexOptions)
      ],
      target: Power,
      controlSignal: Boolean
  ): Seq[(UUID, Power)] = Seq.empty

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: ProvideMinMaxFlexOptions,
  ): ProvideMinMaxFlexOptions = {
    if (controllableAssets.contains(assetInput.getClass))
      flexOptions
    else {
      // device is not controllable by this EmAgent
      flexOptions.copy(
        min = flexOptions.ref,
        max = flexOptions.ref,
      )
    }
  }
}
