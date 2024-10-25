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
final case class PrimaryDataParticipantModel[P <: PrimaryData[_]: ClassTag](
    override val uuid: UUID,
    override val sRated: Power,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    primaryDataResultFunc: PrimaryResultFunc[P],
) extends ParticipantModel[
      PrimaryOperatingPoint[_, P],
      ConstantState.type,
      PrimaryOperationRelevantData[P],
    ]
    with ParticipantConstantModel[PrimaryOperatingPoint[
      _,
      P,
    ], PrimaryOperationRelevantData[
      P
    ]] {

  override def determineOperatingPoint(
      state: ParticipantModel.ConstantState.type,
      relevantData: PrimaryOperationRelevantData[P],
  ): (PrimaryOperatingPoint[_, P], Option[Long]) =
    (PrimaryOperatingPoint(relevantData.data), None)

  override def createResults(
      state: ParticipantModel.ConstantState.type,
      lastOperatingPoint: Option[PrimaryOperatingPoint[_, P]],
      currentOperatingPoint: PrimaryOperatingPoint[_, P],
      complexPower: PrimaryData.ApparentPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] = {
    val primaryDataWithApparentPower = currentOperatingPoint match {
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
  ): PrimaryOperationRelevantData[P] =
    receivedData
      .collectFirst { case data: P =>
        PrimaryOperationRelevantData(data)
      }
      .getOrElse {
        throw new CriticalFailureException(
          s"Expected WeatherData, got $receivedData"
        )
      }

  override def calcFlexOptions(
      state: ParticipantModel.ConstantState.type,
      relevantData: PrimaryOperationRelevantData[P],
  ): FlexibilityMessage.ProvideFlexOptions = {
    val (operatingPoint, _) = determineOperatingPoint(state, relevantData)
    val power = operatingPoint.activePower

    ProvideMinMaxFlexOptions.noFlexOption(uuid, power)
  }

  override def handlePowerControl(
      state: ParticipantModel.ConstantState.type,
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
      // todo relevant data needed
  ): (PrimaryOperatingPoint[_, P], ParticipantModel.ModelChangeIndicator) = {
    ??? // fixme hmmm. scale by amount of setPower in relation to active power
  }

}

object PrimaryDataParticipantModel {

  final case class PrimaryOperationRelevantData[+P <: PrimaryData[_]](data: P)
      extends OperationRelevantData

  trait PrimaryOperatingPoint[T <: PrimaryOperatingPoint[
    T,
    P,
  ], +P <: PrimaryData[_]]
      extends OperatingPoint[PrimaryOperatingPoint[T, P]] {
    val data: P

    override val activePower: Power = data.p
  }

  object PrimaryOperatingPoint {
    def apply[P <: PrimaryData[_]: ClassTag](
        data: P
    ): PrimaryOperatingPoint[_, P] =
      data match {
        case apparentPowerData: PrimaryDataWithApparentPower[_] =>
          PrimaryApparentPowerOperatingPoint(apparentPowerData)
        case other =>
          PrimaryActivePowerOperatingPoint(other)
      }
  }

  private final case class PrimaryApparentPowerOperatingPoint[
      +P <: PrimaryDataWithApparentPower[P]
  ](override val data: P)
      extends PrimaryOperatingPoint[PrimaryApparentPowerOperatingPoint[_], P] {
    override val reactivePower: Option[ReactivePower] = Some(data.q)

    override def zero: PrimaryApparentPowerOperatingPoint[P] =
      copy(data = data.scale(0d))
  }

  private final case class PrimaryActivePowerOperatingPoint[
      +P <: PrimaryData[P] with EnrichableData[P2],
      P2 <: P with PrimaryDataWithApparentPower[P2],
  ](
      override val data: P
  ) extends PrimaryOperatingPoint[PrimaryActivePowerOperatingPoint[_, P2], P] {
    override val reactivePower: Option[ReactivePower] = None

    override def zero: PrimaryActivePowerOperatingPoint[P, P2] =
      copy(data = data.scale(0d))
  }

  /** Function needs to be packaged to be store it in a val
    * @tparam P
    */
  trait PrimaryResultFunc[
      P <: PrimaryData[_]
  ] {
    def createResult(
        data: P with PrimaryDataWithApparentPower[_],
        dateTime: ZonedDateTime,
    ): SystemParticipantResult
  }

}
