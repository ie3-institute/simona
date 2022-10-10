/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.EmAgent.FlexCorrespondence
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.EmModel.{
  EmRelevantData,
  relativeTolerance,
  zeroApparentPower
}
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.QuantityUtil
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import edu.ie3.util.quantities.{QuantityUtil => PsuQuantityUtil}

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}

final case class EmModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl
) extends SystemParticipant[EmRelevantData, ConstantState.type](
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
  ): Seq[(UUID, ComparableQuantity[Power])] = {

    val suggestedPower =
      flexOptions
        .map { case (_, ProvideMinMaxFlexOptions(_, suggestedPower, _, _)) =>
          suggestedPower
        }
        .reduceOption { (power1, power2) =>
          power1.add(power2)
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

    if (
      suggestedPower.isLessThan(QuantityUtil.zero(PowerSystemUnits.KILOWATT))
    ) {
      // excess power, try to store it/increase load

      val orderedParticipants = Seq(evcsOpt, storageOpt, heatPumpOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, ComparableQuantity[Power])], Option(suggestedPower))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          val differenceNoControl =
            flexOption.referencePower.subtract(flexOption.maxPower)

          if (
            PsuQuantityUtil.isEquivalentAbs(
              QuantityUtil.zero(PowerSystemUnits.KILOWATT),
              differenceNoControl,
              relativeTolerance
            )
          ) {
            (issueCtrlMsgs, Some(remainingExcessPower))
          } else if (remainingExcessPower.isLessThan(differenceNoControl)) {
            // we cannot cover the excess feed-in with just this flexibility,
            // thus use all of the flexibility
            (
              issueCtrlMsgs :+ (spi.getUuid, flexOption.maxPower),
              Some(remainingExcessPower.subtract(differenceNoControl))
            )
          } else {
            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl =
              flexOption.referencePower.subtract(remainingExcessPower)

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

      val orderedParticipants = Seq(storageOpt, evcsOpt, heatPumpOpt).flatten

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, ComparableQuantity[Power])], Option(suggestedPower))
      ) {
        case (
              (issueCtrlMsgs, Some(remainingExcessPower)),
              (spi, flexOption: ProvideMinMaxFlexOptions)
            ) =>
          val differenceNoControl =
            flexOption.referencePower.subtract(flexOption.minPower)

          if (
            PsuQuantityUtil.isEquivalentAbs(
              QuantityUtil.zero(PowerSystemUnits.KILOWATT),
              differenceNoControl,
              relativeTolerance
            )
          ) {
            (issueCtrlMsgs, Some(remainingExcessPower))
          } else if (remainingExcessPower.isGreaterThan(differenceNoControl)) {
            // we cannot cover the excess load with just this flexibility,
            // thus use all of the flexibility
            (
              issueCtrlMsgs :+ (spi.getUuid, flexOption.minPower),
              Some(remainingExcessPower.subtract(differenceNoControl))
            )
          } else {
            // this flexibility covers more than we need to reach zero excess,
            // thus we only use as much as we need
            val powerCtrl =
              flexOption.referencePower.subtract(remainingExcessPower)

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

  override def calculatePower(
      tick: Long,
      voltage: ComparableQuantity[Dimensionless],
      data: EmRelevantData
  ): ApparentPower =
    data.flexCorrespondences
      .map { correspondence =>
        correspondence.participantResult
          .map(res => ApparentPower(res.getP, res.getQ))
          .getOrElse(
            throw new RuntimeException(s"No result received in $correspondence")
          )
      }
      .reduceOption { (power1, power2) =>
        ApparentPower(power1.p.add(power2.p), power1.q.add(power2.q))
      }
      .map { power =>
        ApparentPower(
          power.p.to(StandardUnits.ACTIVE_POWER_RESULT),
          power.q.to(StandardUnits.REACTIVE_POWER_RESULT)
        )
      }
      .getOrElse(zeroApparentPower)

  override protected def calculateActivePower(
      data: EmRelevantData
  ): ComparableQuantity[Power] =
    throw new NotImplementedError("Use calculatePower directly")

  override def determineFlexOptions(
      data: EmRelevantData,
      lastState: ModelState.ConstantState.type
  ): FlexibilityMessage.ProvideFlexOptions =
    throw new NotImplementedError("EmModel cannot be managed")

  override def handleControlledPowerChange(
      data: EmRelevantData,
      lastState: ModelState.ConstantState.type,
      setPower: ComparableQuantity[Power]
  ): (ModelState.ConstantState.type, FlexChangeIndicator) =
    throw new NotImplementedError("EmModel cannot be managed")
}

object EmModel {

  private val relativeTolerance = 1e-6d

  private val zeroApparentPower = ApparentPower(
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT)
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
