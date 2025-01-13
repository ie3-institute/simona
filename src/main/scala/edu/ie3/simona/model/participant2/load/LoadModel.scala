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
  ModelState,
}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits.{KILOVOLTAMPERE, KILOWATTHOUR}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.energy.KilowattHours
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

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable.empty

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
      referenceType: LoadReferenceType,
      input: LoadInput,
      maxPower: Power,
      referenceEnergy: Energy,
  ): (Double, ApparentPower) = {
    val sRated = Kilovoltamperes(
      input.getsRated
        .to(KILOVOLTAMPERE)
        .getValue
        .doubleValue
    )
    val eConsAnnual = KilowattHours(
      input.geteConsAnnual().to(KILOWATTHOUR).getValue.doubleValue
    )

    val referenceScalingFactor = referenceType match {
      case LoadReferenceType.ActivePower =>
        val pRated = sRated.toActivePower(input.getCosPhiRated)
        pRated / maxPower
      case LoadReferenceType.EnergyConsumption =>
        eConsAnnual / referenceEnergy
    }

    val scaledSRated = referenceType match {
      case LoadReferenceType.ActivePower =>
        sRated
      case LoadReferenceType.EnergyConsumption =>
        val maxApparentPower = Kilovoltamperes(
          maxPower.toKilowatts / input.getCosPhiRated
        )
        maxApparentPower * referenceScalingFactor
    }

    (referenceScalingFactor, scaledSRated)
  }

  def apply(
      input: LoadInput,
      config: LoadRuntimeConfig,
  ): LoadModel[_ <: ModelState] = {
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
