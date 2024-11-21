/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.result.system.{
  LoadResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.load.LoadModelBehaviour
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ConstantState,
  OperationRelevantData,
  ParticipantConstantModel,
}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.{Energy, Power}

import java.time.ZonedDateTime

abstract class LoadModel[OR <: OperationRelevantData]
    extends ParticipantModel[
      ActivePowerOperatingPoint,
      ConstantState.type,
      OR,
    ]
    with ParticipantConstantModel[
      ActivePowerOperatingPoint,
      OR,
    ]
    with ParticipantSimpleFlexibility[
      ConstantState.type,
      OR,
    ] {

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: ParticipantModel.ConstantState.type,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      new LoadResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new LoadResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable.empty

}

object LoadModel {

  /** Scale profile based load models' sRated based on a provided active power
    * value
    *
    * When the load is scaled to the active power value, the models' sRated is
    * multiplied by the ratio of the provided active power value and the active
    * power value of the model (activePowerVal / (input.sRated*input.cosPhi)
    *
    * @param inputModel
    *   the input model instance
    * @param activePower
    *   the active power value sRated should be scaled to
    * @param safetyFactor
    *   a safety factor to address potential higher sRated values than the
    *   original scaling would provide (e.g. when using unrestricted probability
    *   functions)
    * @return
    *   the inputs model sRated scaled to the provided active power
    */
  def scaleSRatedActivePower(
      inputModel: LoadInput,
      activePower: Power,
      safetyFactor: Double = 1d,
  ): ApparentPower = {
    val sRated = Kilovoltamperes(
      inputModel.getsRated
        .to(PowerSystemUnits.KILOVOLTAMPERE)
        .getValue
        .doubleValue
    )
    val pRated = sRated.toActivePower(inputModel.getCosPhiRated)
    val referenceScalingFactor = activePower / pRated
    sRated * referenceScalingFactor * safetyFactor
  }

  /** Scale profile based load model's sRated based on the provided yearly
    * energy consumption
    *
    * When the load is scaled based on the consumed energy per year, the
    * installed sRated capacity is not usable anymore instead, the load's rated
    * apparent power is scaled on the maximum power occurring in the specified
    * load profile multiplied by the ratio of the annual consumption and the
    * standard load profile scale
    *
    * @param inputModel
    *   the input model instance
    * @param energyConsumption
    *   the yearly energy consumption the models' sRated should be scaled to
    * @param profileMaxPower
    *   the maximum power value of the profile
    * @param profileEnergyScaling
    *   the energy scaling factor of the profile (= amount of yearly energy the
    *   profile is scaled to)
    * @param safetyFactor
    *   a safety factor to address potential higher sRated values than the
    *   original scaling would provide (e.g. when using unrestricted probability
    *   functions)
    * @return
    *   the inputs model sRated scaled to the provided energy consumption
    */
  def scaleSRatedEnergy(
      inputModel: LoadInput,
      energyConsumption: Energy,
      profileMaxPower: Power,
      profileEnergyScaling: Energy,
      safetyFactor: Double = 1d,
  ): ApparentPower = {
    val profileMaxApparentPower = Kilovoltamperes(
      profileMaxPower.toKilowatts / inputModel.getCosPhiRated
    )

    profileMaxApparentPower * (energyConsumption / profileEnergyScaling) * safetyFactor
  }

  def apply(
      input: LoadInput,
      config: LoadRuntimeConfig,
  ): LoadModel[_ <: OperationRelevantData] = {
    LoadModelBehaviour(config.modelBehaviour) match {
      case LoadModelBehaviour.FIX =>
        FixedLoadModel(input, config)
      case LoadModelBehaviour.PROFILE =>
        ProfileLoadModel(input, config)
      case LoadModelBehaviour.RANDOM =>
        RandomLoadModel(input, config)
    }
  }
}
