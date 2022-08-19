/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssuePowerCtrl,
  ProvideMinMaxFlexOptions
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.QuantityUtil
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Power

final case class EmModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    connectedAgents: Map[UUID, SystemParticipantInput]
) extends SystemParticipant[EmRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      Quantities.getQuantity(0, PowerSystemUnits.KILOWATT), // FIXME dummy
      0 // FIXME dummy
    ) {

  /** Determine the power of controllable devices such as storages
    */
  def determineDeviceControl(
      flexOptions: Seq[(SystemParticipantInput, ProvideMinMaxFlexOptions)]
  ): Seq[(UUID, IssuePowerCtrl)] = {

    val suggestedPower =
      QuantityUtil
        .sum(flexOptions.map {
          case (_, ProvideMinMaxFlexOptions(_, suggestedPower, _, _)) =>
            suggestedPower
        })
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

    if (
      suggestedPower.isLessThan(QuantityUtil.zero(PowerSystemUnits.KILOWATT))
    ) {
      // excess power, try to store it/increase load

      val orderedParticipants = Seq(evcsOpt, storageOpt, heatPumpOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, IssuePowerCtrl)], suggestedPower)
      ) {
        case (
              (issueCtrlMsgs, remainingExcessPower),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          val differenceNoControl =
            flexOption.suggestedPower.subtract(flexOption.maxPower)

          if (remainingExcessPower.isLessThan(differenceNoControl)) {
            // we cannot cover the excess feed-in with just this flexibility,
            // thus use all of the flexibility
            (
              issueCtrlMsgs :+ (spi.getUuid, IssuePowerCtrl(
                flexOption.maxPower
              )),
              remainingExcessPower.subtract(differenceNoControl)
            )
          } else {
            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl = flexOption.suggestedPower.add(differenceNoControl)

            (
              issueCtrlMsgs :+ (spi.getUuid, IssuePowerCtrl(powerCtrl)),
              QuantityUtil.zero(PowerSystemUnits.KILOWATT)
            )
          }
      } match {
        case (issueCtrlMsgs, _) => issueCtrlMsgs
      }

    } else {
      // excess load, try to cover it with stored energy/by reducing load

      val orderedParticipants = Seq(storageOpt, evcsOpt, heatPumpOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, IssuePowerCtrl)], suggestedPower)
      ) {
        case (
              (issueCtrlMsgs, remainingExcessPower),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          val differenceNoControl =
            flexOption.suggestedPower.subtract(flexOption.minPower)

          if (remainingExcessPower.isGreaterThan(differenceNoControl)) {
            // we cannot cover the excess load with just this flexibility,
            // thus use all of the flexibility
            (
              issueCtrlMsgs :+ (spi.getUuid, IssuePowerCtrl(
                flexOption.minPower
              )),
              remainingExcessPower.subtract(differenceNoControl)
            )
          } else {
            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl =
              flexOption.suggestedPower.subtract(differenceNoControl)

            (
              issueCtrlMsgs :+ (spi.getUuid, IssuePowerCtrl(powerCtrl)),
              QuantityUtil.zero(PowerSystemUnits.KILOWATT)
            )
          }
      } match {
        case (issueCtrlMsgs, _) => issueCtrlMsgs
      }
    }

  }

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  protected def calculateActivePower(
      data: EmRelevantData
  ): ComparableQuantity[Power] = ???
}

object EmModel {

  /** Class that holds all relevant data for Energy Management calculation
    *
    * @param dateTime
    *   date and time of the <b>ending</b> of time frame to calculate
    */
  final case class EmRelevantData(
      // TODO: From PvModel, Check and refactor
      dateTime: ZonedDateTime,
      lastResults: ValueStore[ApparentPowerAndHeat]
  ) extends CalcRelevantData

  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      connectedAgents: Map[UUID, SystemParticipantInput]
  ): EmModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    new EmModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      modelConfig.scaling,
      QControl(inputModel.getqCharacteristics),
      connectedAgents
    )
  }

}
