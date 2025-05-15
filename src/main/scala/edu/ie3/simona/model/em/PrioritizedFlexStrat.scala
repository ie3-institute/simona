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
  SystemParticipantInput,
  WecInput,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.em.EmModelStrat.tolerance
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import squants.Power

import java.util.UUID

/** Determines flex control for connected agents by adhering to a priority
  * hierarchy, with some devices not controlled at all.
  *
  * @param curtailRegenerative
  *   Whether PV and WEC feed-in can be curtailed or not
  */
final case class PrioritizedFlexStrat(curtailRegenerative: Boolean)
    extends EmModelStrat {

  /** Only heat pumps, battery storages, charging stations and PVs/WECs (if
    * enabled) are controlled by this strategy
    */
  private val controllableAssets: Seq[Class[? <: AssetInput]] =
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
        (? <: AssetInput, MinMaxFlexOptions)
      ],
      target: Power,
  ): Seq[(UUID, Power)] = {

    val totalRefPower =
      flexOptions
        .map { case (_, MinMaxFlexOptions(refPower, _, _)) =>
          refPower
        }
        .reduceOption { (power1, power2) =>
          power1 + power2
        }
        .getOrElse(
          throw new CriticalFailureException(
            "No flexibilities have been provided"
          )
        )

    val targetDelta = totalRefPower - target

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
      .filter(_ => curtailRegenerative) // only if enabled
    val wecOpt = flexOptions
      .collectFirst { case flex @ (_: WecInput, _) =>
        flex
      }
      .filter(_ => curtailRegenerative) // only if enabled

    if zeroKW.~=(targetDelta)(using tolerance) then {
      Seq.empty
    } else if targetDelta < zeroKW then {
      // suggested power too low, try to store difference/increase load

      val orderedParticipants =
        Seq(evcsOpt, storageOpt, heatPumpOpt, pvOpt, wecOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, Power)], Option(targetDelta))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (
                inputModel: SystemParticipantInput,
                flexOption: MinMaxFlexOptions,
              ),
            ) =>
          // potential for decreasing feed-in/increasing load (negative)
          val flexPotential =
            flexOption.ref - flexOption.max

          if zeroKW.~=(remainingExcessPower)(using tolerance) then {
            // we're already there (besides rounding error)
            (issueCtrlMsgs, None)
          } else if zeroKW.~=(flexPotential)(using tolerance) then {
            // device does not offer usable flex potential here
            (issueCtrlMsgs, Some(remainingExcessPower))
          } else if remainingExcessPower < flexPotential then {
            // we cannot cover the excess feed-in with just this flexibility,
            // thus use all the available flexibility and continue
            (
              issueCtrlMsgs :+ (inputModel.getUuid, flexOption.max),
              Some(remainingExcessPower - flexPotential),
            )
          } else {

            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl = flexOption.max.min(
              flexOption.ref - remainingExcessPower
            )

            (
              issueCtrlMsgs :+ (inputModel.getUuid, powerCtrl),
              None,
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

      val orderedParticipants = Seq(storageOpt, evcsOpt, heatPumpOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, Power)], Option(targetDelta))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (
                inputModel: SystemParticipantInput,
                flexOption: MinMaxFlexOptions,
              ),
            ) =>
          // potential for decreasing load/increasing feed-in
          val flexPotential =
            flexOption.ref - flexOption.min

          if zeroKW.~=(remainingExcessPower)(using tolerance) then {
            // we're already there (besides rounding error)
            (issueCtrlMsgs, None)
          } else if zeroKW.~=(flexPotential)(using tolerance) then {
            // device does not offer usable flex potential here
            (issueCtrlMsgs, Some(remainingExcessPower))
          } else if remainingExcessPower > flexPotential then {
            // we cannot cover the excess load with just this flexibility,
            // thus use all the available flexibility and continue
            (
              issueCtrlMsgs :+ (inputModel.getUuid, flexOption.min),
              Some(remainingExcessPower - flexPotential),
            )
          } else {

            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl = flexOption.min.max(
              flexOption.ref - remainingExcessPower
            )

            (
              issueCtrlMsgs :+ (inputModel.getUuid, powerCtrl),
              None,
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

  override def adaptFlexOptions(
      assetInput: AssetInput,
      flexOptions: MinMaxFlexOptions,
  ): MinMaxFlexOptions = {
    if controllableAssets.contains(assetInput.getClass) then flexOptions
    else {
      // device is not controllable by this EmAgent
      flexOptions.copy(
        min = flexOptions.ref,
        max = flexOptions.ref,
      )
    }
  }
}
