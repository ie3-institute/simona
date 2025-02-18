/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelInput,
  ModelState,
}
import edu.ie3.simona.model.participant2.PvModel.PvState
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities._

import java.time.ZonedDateTime
import java.util.UUID

class PvModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      PvState,
    ]
    with ParticipantSimpleFlexibility[PvState] {

  override val initialState: ModelInput => PvState = { input =>
    PvState(
      input.currentTick
    )
  }

  override def determineState(
      lastState: PvState,
      operatingPoint: ActivePowerOperatingPoint,
      input: ModelInput,
  ): PvState = throw new NotImplementedError("Dummy implementation")

  override def determineOperatingPoint(
      state: PvState
  ): (ActivePowerOperatingPoint, Option[Long]) = throw new NotImplementedError(
    "Dummy implementation"
  )

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    throw new NotImplementedError("Dummy implementation")

  override def createResults(
      state: PvState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] = throw new NotImplementedError(
    "Dummy implementation"
  )

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult = throw new NotImplementedError(
    "Dummy implementation"
  )

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable(ServiceType.WeatherService)

}

object PvModel {

  final case class PvState(
      override val tick: Long
  ) extends ModelState

  def apply(
      input: PvInput
  ): PvModel =
    new PvModel(
      input.getUuid,
      input.getId,
      Kilovoltamperes(
        input.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      input.getCosPhiRated,
      QControl(input.getqCharacteristics),
    )

}
