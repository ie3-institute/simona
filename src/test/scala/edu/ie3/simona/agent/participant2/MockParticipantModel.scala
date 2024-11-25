/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant2.MockParticipantModel.MockResult
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  FixedRelevantData,
  FixedState,
  ModelChangeIndicator,
  ParticipantFixedState,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.Dimensionless
import squants.energy.{Kilowatts, Power}
import tech.units.indriya.ComparableQuantity
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble

import javax.measure.quantity.{Power => QuantPower}
import java.time.ZonedDateTime
import java.util.UUID

class MockParticipantModel(
    override val uuid: UUID = UUID.fromString("0-0-0-0-1"),
    override val sRated: ApparentPower = Kilovoltamperes(10),
    override val cosPhiRated: Double = 0.9,
    override val qControl: QControl = CosPhiFixed(0.9),
    mockActivationTicks: Map[Long, Long],
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      FixedState,
      FixedRelevantData.type,
    ]
    with ParticipantFixedState[
      ActivePowerOperatingPoint,
      FixedRelevantData.type,
    ] {

  override def determineOperatingPoint(
      state: FixedState,
      relevantData: FixedRelevantData.type,
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    (
      ActivePowerOperatingPoint(Kilowatts(5)),
      mockActivationTicks.get(state.tick),
    )
  }

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: FixedState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: PrimaryData.ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      MockResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryData.PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult = ???

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable.empty

  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): FixedRelevantData.type = FixedRelevantData

  override def calcFlexOptions(
      state: FixedState,
      relevantData: FixedRelevantData.type,
  ): FlexibilityMessage.ProvideFlexOptions = ???

  override def handlePowerControl(
      state: FixedState,
      relevantData: FixedRelevantData.type,
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (ActivePowerOperatingPoint, ModelChangeIndicator) = ???
}

object MockParticipantModel {

  final case class MockResult(
      time: ZonedDateTime,
      inputModel: UUID,
      p: ComparableQuantity[QuantPower],
      q: ComparableQuantity[QuantPower],
  ) extends SystemParticipantResult(time, inputModel, p, q)

}
