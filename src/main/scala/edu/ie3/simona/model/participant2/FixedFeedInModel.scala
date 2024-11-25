/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.result.system.{
  FixedFeedInResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  FixedState,
  FixedRelevantData,
  ParticipantFixedState,
}
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.Dimensionless

import java.time.ZonedDateTime
import java.util.UUID

class FixedFeedInModel(
    override val uuid: UUID,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      FixedState,
      FixedRelevantData.type,
    ]
    with ParticipantFixedState[
      ActivePowerOperatingPoint,
      FixedRelevantData.type,
    ]
    with ParticipantSimpleFlexibility[
      FixedState,
      FixedRelevantData.type,
    ] {

  override def determineOperatingPoint(
      state: ParticipantModel.FixedState,
      relevantData: ParticipantModel.FixedRelevantData.type,
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    val power = pRated * -1

    (ActivePowerOperatingPoint(power), None)
  }

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: ParticipantModel.FixedState,
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
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new FixedFeedInResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable.empty

  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): FixedRelevantData.type = FixedRelevantData
}

object FixedFeedInModel {
  def apply(
      inputModel: FixedFeedInInput
  ): FixedFeedInModel = {
    new FixedFeedInModel(
      inputModel.getUuid,
      Kilovoltamperes(
        inputModel.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      inputModel.getCosPhiRated,
      QControl.apply(inputModel.getqCharacteristics),
    )
  }
}
