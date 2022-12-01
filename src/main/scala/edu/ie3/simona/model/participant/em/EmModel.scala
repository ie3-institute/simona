/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore.FlexCorrespondence
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.em.EmModel.{
  EmRelevantData,
  zeroApparentPower
}
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  FlexChangeIndicator,
  ModelState,
  SystemParticipant
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}

final case class EmModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    modelStrategy: EmModelStrat
) extends SystemParticipant[EmRelevantData, ApparentPower, ConstantState.type](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      zeroKW, // FIXME dummy
      0 // FIXME dummy
    ) {

  def determineDeviceControl(
      flexOptions: Seq[(_ <: SystemParticipantInput, ProvideMinMaxFlexOptions)],
      target: ComparableQuantity[Power]
  ): Seq[(UUID, ComparableQuantity[Power])] =
    modelStrategy.determineDeviceControl(flexOptions, target)

  override def calculatePower(
      tick: Long,
      voltage: ComparableQuantity[Dimensionless],
      modelState: ConstantState.type,
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
      modelState: ConstantState.type,
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

  val relativeTolerance: Double = 1e-6d

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
      simulationEndDate: ZonedDateTime,
      modelStrategy: EmModelStrat
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
      QControl(inputModel.getqCharacteristics),
      modelStrategy
    )

    model.enable()

    model
  }

}
