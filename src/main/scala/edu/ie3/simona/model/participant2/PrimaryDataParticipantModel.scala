/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  EnrichableData,
  PrimaryDataWithApparentPower,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ConstantState,
  OperatingPoint,
  OperationRelevantData,
  ParticipantConstantModel,
}
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel._
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.ReactivePower
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.ClassTag

/** Just "replaying" primary data
  */
final case class PrimaryDataParticipantModel[T <: PrimaryData: ClassTag](
    override val uuid: UUID,
    override val sRated: Power,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    primaryDataResultFunc: PrimaryResultFunc[T],
) extends ParticipantModel[
      PrimaryOperatingPoint[T],
      ConstantState.type,
      PrimaryOperationRelevantData[T],
    ]
    with ParticipantConstantModel[PrimaryOperatingPoint[
      T
    ], PrimaryOperationRelevantData[
      T
    ]] {

  override def determineOperatingPoint(
      state: ParticipantModel.ConstantState.type,
      relevantData: PrimaryOperationRelevantData[T],
  ): (PrimaryOperatingPoint[T], Option[Long]) =
    (PrimaryOperatingPoint(relevantData.data), None)

  override def createResults(
      state: ParticipantModel.ConstantState.type,
      operatingPoint: PrimaryOperatingPoint[T],
      complexPower: PrimaryData.ApparentPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] = {
    val primaryDataWithApparentPower = operatingPoint match {
      case PrimaryApparentPowerOperatingPoint(data) =>
        data
      case PrimaryActivePowerOperatingPoint(data) =>
        data.add(complexPower.q)
    }
    Iterable(
      primaryDataResultFunc.createResult(primaryDataWithApparentPower, dateTime)
    )
  }

  override def createPrimaryDataResult(
      data: PrimaryDataWithApparentPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult = throw new CriticalFailureException(
    "Method not implemented by this model."
  )

  override def getRequiredServices: Iterable[ServiceType] = {
    // only secondary services should be specified here
    Iterable.empty
  }

  /** @param receivedData
    * @throws CriticalFailureException
    *   if unexpected type of data was provided
    * @return
    */
  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): PrimaryOperationRelevantData[T] =
    receivedData
      .collectFirst { case data: T =>
        PrimaryOperationRelevantData(data)
      }
      .getOrElse {
        throw new CriticalFailureException(
          s"Expected WeatherData, got $receivedData"
        )
      }

  override def calcFlexOptions(
      state: ParticipantModel.ConstantState.type,
      relevantData: PrimaryOperationRelevantData[T],
  ): FlexibilityMessage.ProvideFlexOptions = {
    val (operatingPoint, _) = determineOperatingPoint(state, relevantData)
    val power = operatingPoint.activePower

    ProvideMinMaxFlexOptions.noFlexOption(uuid, power)
  }

  override def handlePowerControl(
      state: ParticipantModel.ConstantState.type,
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (PrimaryOperatingPoint[T], ParticipantModel.ModelChangeIndicator) = {
    ??? // fixme hmmm
  }

}

object PrimaryDataParticipantModel {

  final case class PrimaryOperationRelevantData[+T <: PrimaryData](data: T)
      extends OperationRelevantData

  trait PrimaryOperatingPoint[+T <: PrimaryData] extends OperatingPoint {
    val data: T

    override val activePower: Power = data.p

  }

  object PrimaryOperatingPoint {
    def apply[T <: PrimaryData: ClassTag](data: T): PrimaryOperatingPoint[T] =
      data match {
        case apparentPowerData: PrimaryDataWithApparentPower[_] =>
          PrimaryApparentPowerOperatingPoint(apparentPowerData)
        case other =>
          PrimaryActivePowerOperatingPoint(other)
      }
  }

  private final case class PrimaryApparentPowerOperatingPoint[
      T <: PrimaryDataWithApparentPower[T]
  ](override val data: T)
      extends PrimaryOperatingPoint[T] {
    override val reactivePower: Option[ReactivePower] = Some(data.q)
  }

  private final case class PrimaryActivePowerOperatingPoint[
      +T <: PrimaryData with EnrichableData[T2],
      T2 <: T with PrimaryDataWithApparentPower[T2],
  ](
      override val data: T
  ) extends PrimaryOperatingPoint[T] {
    override val reactivePower: Option[ReactivePower] = None
  }

  /** Function needs to be packaged to be store it in a val
    * @tparam T
    */
  trait PrimaryResultFunc[
      T <: PrimaryData
  ] {
    def createResult(
        data: T with PrimaryDataWithApparentPower[_],
        dateTime: ZonedDateTime,
    ): SystemParticipantResult
  }

}
