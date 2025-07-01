/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.PrimaryDataParticipantModel.PrimaryDataState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKVAr, zeroKW}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.PowerConversionSimona

import java.time.ZonedDateTime
import java.util.UUID

class PrimaryLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
) extends LoadModel[PrimaryDataState[ComplexPower]] {

  override def determineState(
      lastState: PrimaryDataState[ComplexPower],
      operatingPoint: ActivePowerOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): PrimaryDataState[ComplexPower] =
    throw new CriticalFailureException(
      "This model does not perform any calculations and can only be used with primary data!"
    )

  override def determineOperatingPoint(
      state: PrimaryDataState[ComplexPower]
  ): (ActivePowerOperatingPoint, Option[Long]) =
    throw new CriticalFailureException(
      "This model does not perform any calculations and can only be used with primary data!"
    )
}

object PrimaryLoadModel {

  final case class Factory(input: LoadInput)
      extends ParticipantModelFactory[PrimaryDataState[ComplexPower]] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): PrimaryDataState[ComplexPower] =
      PrimaryDataState(
        ComplexPower(zeroKW, zeroKVAr),
        tick,
      )

    override def create(): PrimaryLoadModel =
      new PrimaryLoadModel(
        input.getUuid,
        input.getId,
        input.getsRated.toApparent,
        input.getCosPhiRated,
        QControl.apply(input.getqCharacteristics),
      )
  }
}
