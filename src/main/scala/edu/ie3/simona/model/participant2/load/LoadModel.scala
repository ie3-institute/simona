/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  FixedState,
}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}

import java.time.ZonedDateTime
import java.util.UUID

class LoadModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      FixedState,
    ]
    with ParticipantSimpleFlexibility[FixedState] {

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: FixedState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    throw new NotImplementedError("Dummy implementation")

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    throw new NotImplementedError("Dummy implementation")

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable.empty

  override val initialState: ParticipantModel.ModelInput => FixedState =
    _ => FixedState(-1)

  override def determineState(
      lastState: FixedState,
      operatingPoint: ActivePowerOperatingPoint,
      input: ParticipantModel.ModelInput,
  ): FixedState = throw new NotImplementedError("Dummy implementation")

  override def determineOperatingPoint(
      state: FixedState
  ): (ActivePowerOperatingPoint, Option[Long]) = throw new NotImplementedError(
    "Dummy implementation"
  )
}

object LoadModel {

  def apply(
      input: LoadInput
  ): LoadModel =
    new LoadModel(
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
