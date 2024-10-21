/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
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
import edu.ie3.simona.model.participant2.PrimaryDataModel.PrimaryOperationRelevantData
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.ReactivePower
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.ClassTag

/** Just "replaying" primary data
  */
final case class PrimaryDataModel[T <: PrimaryData: ClassTag](
    override val uuid: UUID,
    override val sRated: Power,
    override val cosPhiRated: Double,
    override val qControl: QControl,
) extends ParticipantModel[
      OperatingPoint,
      ConstantState.type,
      PrimaryOperationRelevantData[T],
    ]
    with ParticipantConstantModel[OperatingPoint, PrimaryOperationRelevantData[
      T
    ]] {

  override def determineOperatingPoint(
      state: ParticipantModel.ConstantState.type,
      relevantData: PrimaryOperationRelevantData[T],
  ): (OperatingPoint, Option[Long]) = ???

  override def createResults(
      lastState: ParticipantModel.ConstantState.type,
      operatingPoint: OperatingPoint,
      complexPower: PrimaryData.ApparentPower,
      dateTime: ZonedDateTime,
  ): ParticipantModel.ResultsContainer = ???

  override def getRequiredServices: Iterable[ServiceType] = ???

  /** @param receivedData
    * @throws CriticalFailureException
    *   if unexpected type of data was provided
    * @return
    */
  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
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
  ): FlexibilityMessage.ProvideFlexOptions = ???

  override def handlePowerControl(
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (OperatingPoint, ParticipantModel.ModelChangeIndicator) = ???
}

object PrimaryDataModel {

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

  private final case class PrimaryActivePowerOperatingPoint[+T <: PrimaryData](
      override val data: T
  ) extends PrimaryOperatingPoint[T] {
    override val reactivePower: Option[ReactivePower] = None
  }
}
