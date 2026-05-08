/*
 * © 2024-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.result.system.{
  FixedFeedInResult,
  SystemParticipantResult,
}
import edu.ie3.simona.model.participant.ParticipantModel.*
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.flex.{
  ParticipantConstantEnergyLimitFlexModel,
  ParticipantFlexModel,
  ParticipantInflexiblePowerLimitFlexModel,
}
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.QuantityUtils.{asMegaVar, asMegaWatt}
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toApparent
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID

class FixedFeedInModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      FixedState,
    ]
    with ParticipantFixedState[ActivePowerOperatingPoint] {

  override val flexModels: Map[FlexType, ParticipantFlexModel[
    ActivePowerOperatingPoint,
    FixedState,
  ]] =
    Map(
      FlexType.PowerLimit -> ParticipantInflexiblePowerLimitFlexModel(this),
      FlexType.EnergyBoundaries -> ParticipantConstantEnergyLimitFlexModel(this),
    )

  override def determineOperatingPoint(
      state: FixedState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    val power = pRated * -1

    (ActivePowerOperatingPoint(power), None)
  }

  override def determineOperatingPoint(
      state: FixedState,
      setPower: Power,
  ): ActivePowerOperatingPoint = ActivePowerOperatingPoint(setPower)

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: FixedState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      new FixedFeedInResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[?],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new FixedFeedInResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

}

object FixedFeedInModel {

  final case class Factory(
      input: FixedFeedInInput
  ) extends ParticipantModelFactory[FixedState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): FixedState = FixedState(tick)

    override def create(): FixedFeedInModel =
      new FixedFeedInModel(
        input.getUuid,
        input.getId,
        input.getsRated.toApparent,
        input.getCosPhiRated,
        QControl.apply(input.getqCharacteristics),
      )

  }

}
