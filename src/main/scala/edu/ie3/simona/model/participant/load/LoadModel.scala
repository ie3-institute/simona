/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.result.system.{
  LoadResult,
  SystemParticipantResult,
}
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant.ParticipantModel
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.util.quantities.QuantityUtils.{asMegaWatt, asMegaVar}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  EnergyToSimona,
  PowerConversionSimona,
}
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.{Energy, Power}

import java.time.ZonedDateTime

abstract class LoadModel[S <: ModelState]
    extends ParticipantModel[
      ActivePowerOperatingPoint,
      S,
    ]
    with ParticipantSimpleFlexibility[S] {

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: S,
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

}

object LoadModel {

  /** Calculates the scaling factor and scaled rated apparent power according to
    * the reference type
    *
    * @param referenceType
    *   The type of reference according to which scaling is calculated
    * @param input
    *   The [[LoadInput]] of the model
    * @param maxPower
    *   The maximum power consumption possible for the model
    * @param referenceEnergy
    *   The (annual) reference energy relevant to the load model
    * @return
    *   the reference scaling factor used for calculation of specific power
    *   consumption values and the scaled rated apparent power
    */
  def scaleToReference(
      referenceType: LoadReferenceType.Value,
      input: LoadInput,
      maxPower: Power,
      referenceEnergy: Energy,
  ): (Double, ApparentPower) = {
    val sRated = input.getsRated.toApparent
    val eConsAnnual = input.geteConsAnnual().toSquants

    val referenceScalingFactor = referenceType match {
      case LoadReferenceType.ACTIVE_POWER =>
        val pRated = sRated.toActivePower(input.getCosPhiRated)
        pRated / maxPower
      case LoadReferenceType.ENERGY_CONSUMPTION =>
        eConsAnnual / referenceEnergy
    }

    val scaledSRated = referenceType match {
      case LoadReferenceType.ACTIVE_POWER =>
        sRated
      case LoadReferenceType.ENERGY_CONSUMPTION =>
        val maxApparentPower = Kilovoltamperes(
          maxPower.toKilowatts / input.getCosPhiRated
        )
        maxApparentPower * referenceScalingFactor
    }

    (referenceScalingFactor, scaledSRated)
  }

  def getFactory(
      input: LoadInput,
      config: LoadRuntimeConfig,
  ): ParticipantModelFactory[_ <: ModelState] =
    LoadModelBehaviour(config.modelBehaviour) match {
      case LoadModelBehaviour.FIX =>
        FixedLoadModel.Factory(input, config)
      case LoadModelBehaviour.PROFILE | LoadModelBehaviour.RANDOM =>
        ProfileLoadModel.Factory(input, config)
    }

}
