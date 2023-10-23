/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.EmAgent.FlexCorrespondence
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.EmModel.{
  EmRelevantData,
  powerTolerance,
  zeroApparentPower
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Kilovars, Megavars}
import squants.energy.{Kilowatts, Megawatts, Watts}
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID

final case class EmModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl
) extends SystemParticipant[EmRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      Kilowatts(0d), // FIXME dummy
      0 // FIXME dummy
    ) {

  /** Determine the power of controllable devices such as storages
    */
  def determineDeviceControl(
      flexOptions: Seq[(SystemParticipantInput, ProvideMinMaxFlexOptions)]
  ): Seq[(UUID, Power)] = {

    val suggestedPower =
      flexOptions
        .map { case (_, ProvideMinMaxFlexOptions(_, suggestedPower, _, _)) =>
          suggestedPower
        }
        .reduceOption { (power1, power2) =>
          power1 + power2
        }
        .getOrElse(throw new RuntimeException("No flexibilities provided"))

    val evcsOpt = flexOptions.collectFirst { case flex @ (_: EvcsInput, _) =>
      flex
    }
    val storageOpt = flexOptions.collectFirst {
      case flex @ (_: StorageInput, _) => flex
    }
    val heatPumpOpt = flexOptions.collectFirst { case flex @ (_: HpInput, _) =>
      flex
    }

    if (suggestedPower < Kilowatts(0d)) {
      // excess power, try to store it/increase load
      val orderedParticipants = Seq(evcsOpt, storageOpt, heatPumpOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, Power)], Option(suggestedPower))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          val differenceNoControl =
            flexOption.referencePower - flexOption.maxPower

          determineFlex(
            true,
            differenceNoControl,
            issueCtrlMsgs,
            remainingExcessPower,
            spi,
            flexOption
          )

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
        (Seq.empty[(UUID, Power)], Option(suggestedPower))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          val differenceNoControl =
            flexOption.referencePower - flexOption.minPower

          determineFlex(
            false,
            differenceNoControl,
            issueCtrlMsgs,
            remainingExcessPower,
            spi,
            flexOption
          )
        case ((issueCtrlMsgs, None), (_, _)) =>
          // if no excess load remains, do nothing
          (issueCtrlMsgs, None)
      } match {
        case (issueCtrlMsgs, _) => issueCtrlMsgs
      }
    }
  }

  /** Determine the power output of provides FlexOptions. Below a
    * \@powerTolerance no FlexOption will be called.
    * @param MaxOptions
    *   true for evaluating against the maximum FlexOption, false for evaluating
    *   against the minimum FlexOption
    * @param differenceNoControl
    *   Represents the power that remains if no control is carried out or the
    *   power to be balanced out
    * @param issueCtrlMsgs
    *
    * @param remainingExcessPower
    *
    * @param spi
    *   SystemParticipantInput
    * @param flexOption
    *   FlexOption under evaluation
    * @return
    */

  def determineFlex(
      maxOptions: Boolean,
      differenceNoControl: Power,
      issueCtrlMsgs: Seq[(UUID, Power)],
      remainingExcessPower: Power,
      spi: SystemParticipantInput,
      flexOption: ProvideMinMaxFlexOptions
  ) = {
    differenceNoControl match {

      case diff if diff.abs < powerTolerance =>
        (issueCtrlMsgs, Some(remainingExcessPower))

      case diff
          if maxOptions && diff > remainingExcessPower && diff.abs >= powerTolerance =>
        // we cannot cover the excess feed-in with just this flexibility,
        // thus use all of the flexibility
        (
          issueCtrlMsgs :+ (spi.getUuid, flexOption.maxPower),
          Some(remainingExcessPower - differenceNoControl)
        )
      case diff
          if maxOptions && diff <= remainingExcessPower && diff.abs >= powerTolerance =>
        // this flexibility covers more than we need to reach zero excess,
        // thus we only use as much as we need
        val powerCtrl =
          flexOption.referencePower - remainingExcessPower
        (
          issueCtrlMsgs :+ (spi.getUuid, powerCtrl),
          None
        )

      case diff
          if !maxOptions && diff < remainingExcessPower && diff.abs >= powerTolerance =>
        // we cannot cover the excess load with just this flexibility,
        // thus use all of the flexibility
        (
          issueCtrlMsgs :+ (spi.getUuid, flexOption.minPower),
          Some(remainingExcessPower - differenceNoControl)
        )
      case diff
          if !maxOptions && diff >= remainingExcessPower && diff.abs >= powerTolerance =>
        // this flexibility covers more than we need to reach zero excess,
        // thus we only use as much as we need
        val powerCtrl =
          flexOption.referencePower - remainingExcessPower

        (
          issueCtrlMsgs :+ (spi.getUuid, powerCtrl),
          None
        )

    }
  }

  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      data: EmRelevantData
  ): ApparentPower =
    data.flexCorrespondences
      .map { correspondence =>
        correspondence.participantResult
          .map(res =>
            ApparentPower(
              Kilowatts(
                res.getP.to(PowerSystemUnits.KILOWATT).getValue.doubleValue()
              ),
              Kilovars(
                res.getQ.to(PowerSystemUnits.KILOVAR).getValue.doubleValue()
              )
            )
          )
          .getOrElse(
            throw new RuntimeException(s"No result received in $correspondence")
          )
      }
      .reduceOption { (power1, power2) =>
        ApparentPower(power1.p + power2.p, power1.q + power2.q)
      }
      .map { power: ApparentPower =>
        ApparentPower(
          power.p,
          power.q
        )
      }
      .getOrElse(zeroApparentPower)

  override protected def calculateActivePower(
      data: EmRelevantData
  ): Power =
    throw new NotImplementedError("Use calculatePower directly")
}

object EmModel {

  private val powerTolerance: Power = Watts(1d)

  private val zeroApparentPower = ApparentPower(
    Megawatts(0d),
    Megavars(0d)
  )

  /** Class that holds all relevant data for Energy Management calculation
    */
  final case class EmRelevantData(
      flexCorrespondences: Iterable[FlexCorrespondence]
  ) extends CalcRelevantData

  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EmModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    val model = new EmModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      modelConfig.scaling,
      QControl(inputModel.getqCharacteristics)
    )

    model.enable()

    model
  }

}
