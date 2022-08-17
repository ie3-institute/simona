/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system._
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueChargingPower,
  IssueFlexibilityControl,
  ProvideFlexibilityOptions,
  ProvideStorageState
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.QuantityUtil
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Power
import scala.reflect.ClassTag

final case class EmModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    uncontrolledAgents: Map[UUID, SystemParticipantInput],
    controlledAgents: Map[UUID, SystemParticipantInput]
) extends SystemParticipant[EmRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      Quantities.getQuantity(0, PowerSystemUnits.KILOWATT), // FIXME dummy
      0 // FIXME dummy
    ) {

  /** TODO maybe use UUIDs instead of ActorRefs here, since EmModel is not
    * supposed to send msgs itself
    *
    * Determine the power of controllable devices such as storages
    * @return
    */
  def determineDeviceControl(
      assetPowers: Map[UUID, SystemParticipantResult],
      flexOptions: Map[UUID, ProvideFlexibilityOptions]
  ): Seq[(UUID, IssueFlexibilityControl)] = {
    val uncontrolledPower = assetPowers.values.foldLeft(
      QuantityUtil.zero(PowerSystemUnits.KILOWATT)
    ) { case (sum, result) =>
      val apparentPower = Math
        .sqrt(
          Math.pow(
            result.getP.to(PowerSystemUnits.KILOWATT).getValue.doubleValue,
            2
          ) +
            Math.pow(
              result.getQ.to(PowerSystemUnits.KILOWATT).getValue.doubleValue,
              2
            )
        )
        .asKiloWatt
      sum.add(apparentPower)
    }

    val evcsOpt = getControlled[EvcsInput](flexOptions)
    val storageOpt = getControlled[StorageInput](flexOptions)
    val heatPumpOpt = getControlled[HpInput](flexOptions)

    val orderedParticipants = Seq(evcsOpt, storageOpt, heatPumpOpt).flatten

    if (
      uncontrolledPower.isLessThan(QuantityUtil.zero(PowerSystemUnits.KILOWATT))
    ) {
      // excess power, try to store it

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, IssueFlexibilityControl)], uncontrolledPower)
      ) {
        case (
              (issueCtrlMsgs, remainingPower),
              (spi, storageState: ProvideStorageState)
            ) =>
          val maxChargingPower = spi match {
            case si: StorageInput =>
              si.getType.getsRated()
            case evcsInput: EvcsInput =>
              evcsInput.getType.getsRated()
            case hpi: HpInput =>
              hpi.getType.getsRated()
          }

          maybeChargeParticipant(
            remainingPower,
            storageState,
            maxChargingPower
          ) match {
            case (issueCtrlOpt, newRemainingPower) =>
              val newIssueCtrlMsgs = issueCtrlOpt
                .map { issueCtrl =>
                  issueCtrlMsgs :+ (spi.getUuid, issueCtrl)
                }
                .getOrElse(issueCtrlMsgs)
              (newIssueCtrlMsgs, newRemainingPower)
          }

        case unexpected =>
          throw new RuntimeException(
            s"Received unexpected flex message $unexpected"
          )
      } match {
        case (issueCtrlMsgs, _) => issueCtrlMsgs
      }
    } else {
      // excess load, try to cover it with storage

      orderedParticipants.foldLeft(
        (Seq.empty[(UUID, IssueFlexibilityControl)], uncontrolledPower)
      ) {
        case (
              (issueCtrlMsgs, remainingPower),
              (spi, storageState: ProvideStorageState)
            ) =>
          val maxChargingPower = spi match {
            case si: StorageInput =>
              si.getType.getsRated()
            case evcsInput: EvcsInput =>
              evcsInput.getType.getsRated()
            case hpi: HpInput =>
              hpi.getType.getsRated()
          }

          maybeDischargeParticipant(
            remainingPower,
            storageState,
            maxChargingPower
          ) match {
            case (issueCtrlOpt, newRemainingPower) =>
              val newIssueCtrlMsgs = issueCtrlOpt
                .map { issueCtrl =>
                  issueCtrlMsgs :+ (spi.getUuid, issueCtrl)
                }
                .getOrElse(issueCtrlMsgs)
              (newIssueCtrlMsgs, newRemainingPower)
          }

        case unexpected =>
          throw new RuntimeException(
            s"Received unexpected flex message $unexpected"
          )
      } match {
        case (issueCtrlMsgs, _) => issueCtrlMsgs
      }
    }
  }

  private def maybeChargeParticipant(
      remainingPower: ComparableQuantity[Power],
      storageState: ProvideStorageState,
      maxChargingPower: ComparableQuantity[Power]
  ): (Option[IssueChargingPower], ComparableQuantity[Power]) = {
    require(
      remainingPower.isLessThanOrEqualTo(
        QuantityUtil.zero(PowerSystemUnits.KILOWATT)
      ),
      () => s"Remaining power is not negative but $remainingPower"
    )

    val leftToFull = storageState.capacity.subtract(storageState.storedEnergy)

    if (
      remainingPower.isLessThan(QuantityUtil.zero(PowerSystemUnits.KILOWATT)) &&
      leftToFull.isGreaterThan(
        QuantityUtil.zero(PowerSystemUnits.KILOWATTHOUR)
      )
    ) {
      if (maxChargingPower.isLessThan(remainingPower.multiply(-1))) {
        (
          Some(IssueChargingPower(maxChargingPower)),
          remainingPower.add(maxChargingPower)
        )
      } else {
        (
          Some(IssueChargingPower(remainingPower.multiply(-1))),
          QuantityUtil.zero(PowerSystemUnits.KILOWATT)
        )
      }

    } else
      (None, remainingPower)
  }

  private def maybeDischargeParticipant(
      remainingPower: ComparableQuantity[Power],
      storageState: ProvideStorageState,
      maxChargingPower: ComparableQuantity[Power]
  ): (Option[IssueChargingPower], ComparableQuantity[Power]) = {
    require(
      remainingPower.isGreaterThanOrEqualTo(
        QuantityUtil.zero(PowerSystemUnits.KILOWATT)
      ),
      () => s"Remaining power is not positive but $remainingPower"
    )

    if (
      remainingPower.isGreaterThan(
        QuantityUtil.zero(PowerSystemUnits.KILOWATT)
      ) &&
      storageState.storedEnergy.isGreaterThan(
        QuantityUtil.zero(PowerSystemUnits.KILOWATTHOUR)
      )
    ) {
      if (maxChargingPower.isLessThan(remainingPower)) {
        val dischargingPower = maxChargingPower.multiply(-1)

        (
          Some(IssueChargingPower(dischargingPower)),
          remainingPower.add(dischargingPower)
        )
      } else {
        (
          Some(IssueChargingPower(remainingPower.multiply(-1))),
          QuantityUtil.zero(PowerSystemUnits.KILOWATT)
        )
      }

    } else
      (None, remainingPower)
  }

  private def getControlled[T <: SystemParticipantInput: ClassTag](
      flexOptions: Map[UUID, ProvideFlexibilityOptions]
  ): Option[(T, ProvideFlexibilityOptions)] =
    flexOptions
      .flatMap { case (uuid, flex) =>
        controlledAgents
          .get(uuid)
          .collectFirst { case si: T =>
            si
          }
          .map(_ -> flex)
      }
      .toSeq
      .headOption // only retrieving the first of given type for now

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
      uncontrolledAgents: Map[UUID, SystemParticipantInput],
      controlledAgents: Map[UUID, SystemParticipantInput]
  ): EmModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    EmModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      modelConfig.scaling,
      QControl(inputModel.getqCharacteristics),
      uncontrolledAgents,
      controlledAgents
    )
  }

}
