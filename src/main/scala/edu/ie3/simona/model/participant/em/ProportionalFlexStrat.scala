/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.model.participant.em.EmModelStrat.tolerance
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import squants.energy.Kilowatts

import java.util.UUID

object ProportionalFlexStrat extends EmModelStrat {

  /** Determine the power of controllable devices by proportionally distributing
    * flexibility usage to connected devices. This means that all devices are
    * set to use the same share of their respective flexibility to reach target
    * power.
    *
    * @param spiFlexOptions
    *   The flex options per connected system participant
    * @param target
    *   The target power to aim for when utilizing flexibility
    * @return
    *   Power set points for devices, if applicable
    */
  override def determineDeviceControl(
      spiFlexOptions: Iterable[
        (_ <: SystemParticipantInput, ProvideMinMaxFlexOptions)
      ],
      target: squants.Power
  ): Iterable[(UUID, squants.Power)] = {

    // SPIs are not needed here
    val flexOptions = spiFlexOptions
      .map { case (spi, flexOptions) =>
        spi.getUuid -> flexOptions
      }

    // sum up reference, minimum and maximum power of all connected devices
    val (totalRef, totalMin, totalMax) = flexOptions
      .foldLeft(
        (Kilowatts(0d), Kilowatts(0d), Kilowatts(0d))
      ) {
        case (
              (sumRef, sumMin, sumMax),
              (_, ProvideMinMaxFlexOptions(_, addRef, addMin, addMax))
            ) =>
          (
            sumRef + addRef,
            sumMin + addMin,
            sumMax + addMax
          )
      }

    if (target.~=(totalRef)(tolerance)) {
      Seq.empty
    } else if (target < totalRef) {
      val reducedOptions = flexOptions.map {
        case (uuid, ProvideMinMaxFlexOptions(_, refPower, minPower, _)) =>
          (uuid, refPower, minPower)
      }

      distributeFlexibility(target, totalRef, totalMin, reducedOptions)
    } else {
      val reducedOptions = flexOptions.map {
        case (uuid, ProvideMinMaxFlexOptions(_, refPower, _, maxPower)) =>
          (uuid, refPower, maxPower)
      }

      distributeFlexibility(target, totalRef, totalMax, reducedOptions)
    }
  }

  /** Proportionally distributes flexibility to given devices
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
      target: squants.Power,
      totalRef: squants.Power,
      totalLimit: squants.Power,
      options: Iterable[(UUID, squants.Power, squants.Power)]
  ): Iterable[(UUID, squants.Power)] = {
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
      val normalizedLimit = totalLimit - totalRef
      val normalizedTarget = target - totalRef

      val flexShare = normalizedTarget / normalizedLimit

      filteredOptions.map { case (uuid, refPower, limitPower) =>
        val diffLimitRef = limitPower - refPower

        // add the required share of flexibility to the reference power
        val setPower = refPower + (diffLimitRef * flexShare)

        uuid -> setPower
      }
    }
  }

  override def adaptFlexOptions(
      spi: SystemParticipantInput,
      flexOptions: ProvideMinMaxFlexOptions
  ): ProvideMinMaxFlexOptions =
    flexOptions
}
