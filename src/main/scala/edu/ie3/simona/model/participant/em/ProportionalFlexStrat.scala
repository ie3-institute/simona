/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.model.participant.em.EmModel.relativeTolerance
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.{QuantityUtil => PsuQuantityUtil}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import tech.units.indriya.{AbstractUnit, ComparableQuantity}

import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}

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
      spiFlexOptions: Seq[
        (_ <: SystemParticipantInput, ProvideMinMaxFlexOptions)
      ],
      target: ComparableQuantity[Power]
  ): Seq[(UUID, ComparableQuantity[Power])] = {

    // SPIs are not needed here
    val flexOptions = spiFlexOptions
      .map { case (_, flexOptions) =>
        flexOptions
      }

    // sum up reference, minimum and maximum power of all connected devices
    val (totalRef, totalMin, totalMax) = flexOptions
      .foldLeft(
        (zeroKW, zeroKW, zeroKW)
      ) {
        case (
              (sumRef, sumMin, sumMax),
              ProvideMinMaxFlexOptions(_, addRef, addMin, addMax)
            ) =>
          (
            sumRef.add(addRef),
            sumMin.add(addMin),
            sumMax.add(addMax)
          )
      }

    if (
      PsuQuantityUtil.isEquivalentAbs(
        target,
        totalRef,
        relativeTolerance
      )
    ) {
      Seq.empty
    } else if (target.isLessThan(totalRef)) {
      val reducedOptions = flexOptions.map {
        case ProvideMinMaxFlexOptions(uuid, refPower, minPower, _) =>
          (uuid, refPower, minPower)
      }

      distributeFlexibility(target, totalRef, totalMin, reducedOptions)
    } else {
      val reducedOptions = flexOptions.map {
        case ProvideMinMaxFlexOptions(uuid, refPower, _, maxPower) =>
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
      target: ComparableQuantity[Power],
      totalRef: ComparableQuantity[Power],
      totalLimit: ComparableQuantity[Power],
      options: Seq[(UUID, ComparableQuantity[Power], ComparableQuantity[Power])]
  ): Seq[(UUID, ComparableQuantity[Power])] = {
    // filter out options with ref == limit because they're useless here
    val filteredOptions = options.filterNot { case (_, refPower, limitPower) =>
      PsuQuantityUtil.isEquivalentAbs(
        refPower,
        limitPower,
        relativeTolerance
      )
    }

    if (
      (target.isLessThan(totalRef) &&
        target.isLessThanOrEqualTo(totalLimit)) ||
      (target.isGreaterThan(totalRef) &&
        target.isGreaterThanOrEqualTo(totalLimit))
    ) {
      // target is beyond limit, thus use limit powers for all applicable devices
      filteredOptions.map { case (uuid, _, limitPower) =>
        uuid -> limitPower
      }
    } else {
      // calculate share of flexibility that each device should carry
      val normalizedLimit = totalLimit.subtract(totalRef)
      val normalizedTarget = target.subtract(totalRef)

      val flexShare = normalizedTarget
        .divide(normalizedLimit)
        .asType(classOf[Dimensionless])
        .to(AbstractUnit.ONE)
        .getValue
        .doubleValue

      filteredOptions.map { case (uuid, refPower, limitPower) =>
        val diffLimitRef = limitPower.subtract(refPower)

        // add the required share of flexibility to the reference power
        val setPower = refPower.add(
          diffLimitRef.multiply(flexShare)
        )

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
