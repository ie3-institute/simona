/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, PrimaryDataMeta}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  EnrichableData,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelInput,
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel._
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.{ApparentPower, ReactivePower}
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.ClassTag

/** Just "replaying" primary data
  */
final case class PrimaryDataParticipantModel[P <: PrimaryData: ClassTag](
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    primaryDataResultFunc: PrimaryResultFunc,
    primaryDataMeta: PrimaryDataMeta[P],
) extends ParticipantModel[
      PrimaryOperatingPoint[P],
      PrimaryDataState[P],
    ] {

  override val initialState: ModelInput => PrimaryDataState[P] = { input =>
    val primaryData = getPrimaryData(input.receivedData)
    PrimaryDataState(
      primaryData,
      input.currentTick,
    )
  }

  override def determineState(
      lastState: PrimaryDataState[P],
      operatingPoint: PrimaryOperatingPoint[P],
      input: ParticipantModel.ModelInput,
  ): PrimaryDataState[P] = initialState(input)

  private def getPrimaryData(receivedData: Seq[Data]): P = {
    receivedData
      .collectFirst { case data: P =>
        data
      }
      .getOrElse {
        throw new CriticalFailureException(
          "Expected primary data of type " +
            s"${implicitly[ClassTag[P]].runtimeClass.getSimpleName}, " +
            s"got $receivedData"
        )
      }
  }

  override def determineOperatingPoint(
      state: PrimaryDataState[P]
  ): (PrimaryOperatingPoint[P], Option[Long]) =
    (PrimaryOperatingPoint(state.data), None)

  override def zeroPowerOperatingPoint: PrimaryOperatingPoint[P] =
    PrimaryOperatingPoint(primaryDataMeta.zero)

  override def createResults(
      state: PrimaryDataState[P],
      lastOperatingPoint: Option[PrimaryOperatingPoint[P]],
      currentOperatingPoint: PrimaryOperatingPoint[P],
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] = {
    val primaryDataWithApparentPower = currentOperatingPoint.data match {
      case primaryDataWithApparentPower: PrimaryDataWithComplexPower[_] =>
        primaryDataWithApparentPower
      case enrichableData: EnrichableData[_] =>
        enrichableData.add(complexPower.q)
    }
    Iterable(
      primaryDataResultFunc.createResult(primaryDataWithApparentPower, dateTime)
    )
  }

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult = throw new CriticalFailureException(
    "Method not implemented by this model."
  )

  override def getRequiredSecondaryServices: Iterable[ServiceType] = {
    // only secondary services should be specified here
    Iterable.empty
  }

  override def determineFlexOptions(
      state: PrimaryDataState[P]
  ): FlexibilityMessage.ProvideFlexOptions = {
    val (operatingPoint, _) = determineOperatingPoint(state)

    ProvideMinMaxFlexOptions.noFlexOption(uuid, operatingPoint.activePower)
  }

  override def determineOperatingPoint(
      state: PrimaryDataState[P],
      setPower: Power,
  ): (PrimaryOperatingPoint[P], OperationChangeIndicator) = {
    val factor = state.data.p / setPower
    val scaledData: P = primaryDataMeta.scale(state.data, factor)

    (PrimaryOperatingPoint(scaledData), OperationChangeIndicator())
  }

}

object PrimaryDataParticipantModel {

  final case class PrimaryDataState[+P <: PrimaryData](
      data: P,
      override val tick: Long,
  ) extends ModelState

  trait PrimaryOperatingPoint[+P <: PrimaryData] extends OperatingPoint {
    val data: P

    override val activePower: Power = data.p
  }

  private object PrimaryOperatingPoint {
    def apply[P <: PrimaryData: ClassTag](
        data: P
    ): PrimaryOperatingPoint[P] =
      data match {
        case apparentPowerData: P with PrimaryDataWithComplexPower[_] =>
          PrimaryApparentPowerOperatingPoint(apparentPowerData)
        case other: P with EnrichableData[_] =>
          PrimaryActivePowerOperatingPoint(other)
      }
  }

  private final case class PrimaryApparentPowerOperatingPoint[
      P <: PrimaryDataWithComplexPower[_]
  ](override val data: P)
      extends PrimaryOperatingPoint[P] {
    override val reactivePower: Option[ReactivePower] = Some(data.q)
  }

  private final case class PrimaryActivePowerOperatingPoint[
      PE <: PrimaryData with EnrichableData[_]: ClassTag
  ](
      override val data: PE
  ) extends PrimaryOperatingPoint[PE] {
    override val reactivePower: Option[ReactivePower] = None
  }

  /** Function needs to be packaged to be store it in a val
    */
  trait PrimaryResultFunc {
    def createResult(
        data: PrimaryDataWithComplexPower[_],
        dateTime: ZonedDateTime,
    ): SystemParticipantResult
  }

}
