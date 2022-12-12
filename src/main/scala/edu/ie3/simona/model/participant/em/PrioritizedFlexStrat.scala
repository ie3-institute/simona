/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.{
  EvcsInput,
  HpInput,
  PvInput,
  StorageInput,
  SystemParticipantInput
}
import edu.ie3.simona.model.participant.em.EmModel.relativeTolerance
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.QuantityUtils.RichQuantity
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.quantities.{QuantityUtil => PsuQuantityUtil}
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Power

final case class PrioritizedFlexStrat(pvFlex: Boolean) extends EmModelStrat {

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
  override def determineDeviceControl(
      flexOptions: Seq[(_ <: SystemParticipantInput, ProvideMinMaxFlexOptions)],
      target: ComparableQuantity[Power]
  ): Seq[(UUID, ComparableQuantity[Power])] = {

    val totalRefPower =
      flexOptions
        .map { case (_, ProvideMinMaxFlexOptions(_, refPower, _, _)) =>
          refPower
        }
        .reduceOption { (power1, power2) =>
          power1.add(power2)
        }
        .getOrElse(throw new RuntimeException("No flexibilities provided"))

    val targetDelta = totalRefPower.subtract(target)

    val evcsOpt = flexOptions.collectFirst { case flex @ (_: EvcsInput, _) =>
      flex
    }
    val storageOpt = flexOptions.collectFirst {
      case flex @ (_: StorageInput, _) => flex
    }
    val heatPumpOpt = flexOptions.collectFirst { case flex @ (_: HpInput, _) =>
      flex
    }
    val pvOpt = flexOptions
      .collectFirst { case flex @ (_: PvInput, _) =>
        flex
      }
      .filter(_ => pvFlex) // only if enabled

    if (
      PsuQuantityUtil.isEquivalentAbs(
        zeroKW,
        targetDelta,
        relativeTolerance
      )
    ) {
      Seq.empty
    } else if (targetDelta.isLessThan(zeroKW)) {
      // suggested power too low, try to store difference/increase load

      // TODO configurable
      val orderedParticipants =
        Seq(evcsOpt, storageOpt, heatPumpOpt, pvOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, ComparableQuantity[Power])], Option(targetDelta))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          // potential for decreasing feed-in/increasing load (negative)
          val flexPotential =
            flexOption.referencePower.subtract(flexOption.maxPower)

          if (
            PsuQuantityUtil.isEquivalentAbs(
              zeroKW,
              remainingExcessPower,
              relativeTolerance
            )
          ) {
            // we're already there (besides rounding error)
            (issueCtrlMsgs, None)
          } else if (
            PsuQuantityUtil.isEquivalentAbs(
              zeroKW,
              flexPotential,
              relativeTolerance
            )
          ) {
            // device does not offer usable flex potential here
            (issueCtrlMsgs, Some(remainingExcessPower))
          } else if (remainingExcessPower.isLessThan(flexPotential)) {
            // we cannot cover the excess feed-in with just this flexibility,
            // thus use all of the available flexibility and continue
            (
              issueCtrlMsgs :+ (spi.getUuid, flexOption.maxPower),
              Some(remainingExcessPower.subtract(flexPotential))
            )
          } else {

            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl = flexOption.maxPower.min(
              flexOption.referencePower.subtract(remainingExcessPower)
            )

            (
              issueCtrlMsgs :+ (spi.getUuid, powerCtrl),
              None
            )
          }
        case ((issueCtrlMsgs, None), (_, _)) =>
          // if no excess feed-in remains, do nothing
          (issueCtrlMsgs, None)
      } match {
        case (issueCtrlMsgs, _) => issueCtrlMsgs
      }

    } else {
      // excess load, try to cover it with stored energy/by reducing load

      // TODO configurable
      val orderedParticipants = Seq(storageOpt, evcsOpt, heatPumpOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, ComparableQuantity[Power])], Option(targetDelta))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          // potential for decreasing load/increasing feed-in
          val flexPotential =
            flexOption.referencePower.subtract(flexOption.minPower)

          if (
            PsuQuantityUtil.isEquivalentAbs(
              zeroKW,
              remainingExcessPower,
              relativeTolerance
            )
          ) {
            // we're already there (besides rounding error)
            (issueCtrlMsgs, None)
          } else if (
            PsuQuantityUtil.isEquivalentAbs(
              zeroKW,
              flexPotential,
              relativeTolerance
            )
          ) {
            // device does not offer usable flex potential here
            (issueCtrlMsgs, Some(remainingExcessPower))
          } else if (remainingExcessPower.isGreaterThan(flexPotential)) {
            // we cannot cover the excess load with just this flexibility,
            // thus use all of the available flexibility and continue
            (
              issueCtrlMsgs :+ (spi.getUuid, flexOption.minPower),
              Some(remainingExcessPower.subtract(flexPotential))
            )
          } else {

            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl = flexOption.minPower.max(
              flexOption.referencePower.subtract(remainingExcessPower)
            )

            (
              issueCtrlMsgs :+ (spi.getUuid, powerCtrl),
              None
            )
          }
        case ((issueCtrlMsgs, None), (_, _)) =>
          // if no excess load remains, do nothing
          (issueCtrlMsgs, None)
      } match {
        case (issueCtrlMsgs, _) => issueCtrlMsgs
      }
    }

  }

  // TODO provide test
  override def adaptFlexOptions(
      spi: SystemParticipantInput,
      flexOptions: ProvideMinMaxFlexOptions
  ): ProvideMinMaxFlexOptions = {
    // only heat pumps, battery storages and charging
    // stations are controlled by this strategy
    // TODO configurable
    val controllableAssets: Seq[Class[_ <: SystemParticipantInput]] =
      Seq(classOf[HpInput], classOf[StorageInput], classOf[EvcsInput]) ++ Option
        .when(pvFlex)(Seq(classOf[PvInput]))
        .getOrElse(Seq.empty)

    if (controllableAssets.contains(spi.getClass))
      flexOptions
    else {
      // device is not controllable by this EmAgent
      flexOptions.copy(
        minPower = flexOptions.referencePower,
        maxPower = flexOptions.referencePower
      )
    }
  }
}
