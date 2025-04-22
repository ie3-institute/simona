/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import EmModelStrat.tolerance
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import squants.Power

import java.util.UUID

/** Proportionally distributes flex control among connected agents, i.e. all
  * agents contribute the same share of their offered flex options
  */
object ProportionalFlexStrat extends EmModelStrat {

  /** Determine the power of controllable devices by proportionally distributing
    * flexibility usage to connected devices. This means that all devices are
    * set to use the same share of their respective flexibility to reach target
    * power.
    *
    * @param modelFlexOptions
    *   The flex options per connected agent
    * @param target
    *   The target power to aim for when utilizing flexibility
    * @return
    *   Power set points for devices, if applicable
    */
  override def determineFlexControl(
      modelFlexOptions: Iterable[
        (_ <: AssetInput, MinMaxFlexOptions)
      ],
      target: Power,
  ): Iterable[(UUID, Power)] = {

    // Input models are not needed here
    val flexOptions = modelFlexOptions
      .map { case (asset, flexOptions) =>
        asset.getUuid -> flexOptions
      }

    // sum up reference, minimum and maximum power of all connected devices
    val totalOptions = flexOptions.map { case (_, flexOptions) =>
      flexOptions
    }.flexSum

    if (target.~=(totalOptions.ref)(tolerance)) {
      Seq.empty
    } else if (target < totalOptions.ref) {
      val reducedOptions = flexOptions.map {
        case (uuid, MinMaxFlexOptions(refPower, minPower, _)) =>
          (uuid, refPower, minPower)
      }

      distributeFlexibility(
        target,
        totalOptions.ref,
        totalOptions.min,
        reducedOptions,
      )
    } else {
      val reducedOptions = flexOptions.map {
        case (uuid, MinMaxFlexOptions(refPower, _, maxPower)) =>
          (uuid, refPower, maxPower)
      }

      distributeFlexibility(
        target,
        totalOptions.ref,
        totalOptions.max,
        reducedOptions,
      )
    }
  }

  /** Proportionally distributes flexibility to given devices
    *
    * @param target
    *   The target power to aim for when utilizing flexibility
    * @param totalRef
    *   The total reference power of all connected devices
    * @param totalLimit
    *   The total limit of power (either positive or negative) of all connected
    *   devices
    * @param options
    *   The flexibility options (model UUID, reference power and limit power)
    * @return
    *   Power set points for devices, if applicable
    */
  private def distributeFlexibility(
      target: Power,
      totalRef: Power,
      totalLimit: Power,
      options: Iterable[(UUID, Power, Power)],
  ): Iterable[(UUID, Power)] = {
    // filter out options with ref == limit because they're useless here
    val filteredOptions = options.filterNot { case (_, refPower, limitPower) =>
      refPower.~=(limitPower)(tolerance)
    }

    if (
      (target < totalRef && target <= totalLimit) ||
      (target > totalRef && target >= totalLimit)
    ) {
      // target is beyond limit, thus use limit powers for all applicable devices
      filteredOptions.map { case (uuid, _, limitPower) =>
        uuid -> limitPower
      }
    } else {
      // calculate share of flexibility that each device should carry
      val deltaToLimit = totalLimit - totalRef
      val deltaToTarget = target - totalRef

      val flexShare = deltaToTarget / deltaToLimit

      filteredOptions.map { case (uuid, refPower, limitPower) =>
        val diffLimitRef = limitPower - refPower

        // add the required share of flexibility to the reference power
        val setPower = refPower + (diffLimitRef * flexShare)

        uuid -> setPower
      }
    }
  }

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: MinMaxFlexOptions,
  ): MinMaxFlexOptions =
    flexOptions
}
