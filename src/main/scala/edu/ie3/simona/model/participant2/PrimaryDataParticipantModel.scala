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
  FixedState,
  OperationChangeIndicator,
  OperatingPoint,
  OperationRelevantData,
  ParticipantFixedState,
}
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel._
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.scala.quantities.{ApparentPower, ReactivePower}
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.ClassTag

/** Just "replaying" primary data
  */
final case class PrimaryDataParticipantModel[P <: PrimaryData: ClassTag](
    override val uuid: UUID,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    primaryDataResultFunc: PrimaryResultFunc,
    primaryDataMeta: PrimaryDataMeta[P],
) extends ParticipantModel[
      PrimaryOperatingPoint[P],
      FixedState,
      PrimaryOperationRelevantData[P],
    ]
    with ParticipantFixedState[
      PrimaryOperatingPoint[P],
      PrimaryOperationRelevantData[P],
    ] {

  override def determineOperatingPoint(
      state: FixedState,
      relevantData: PrimaryOperationRelevantData[P],
  ): (PrimaryOperatingPoint[P], Option[Long]) =
    (PrimaryOperatingPoint(relevantData.data), None)

  override def zeroPowerOperatingPoint: PrimaryOperatingPoint[P] =
    PrimaryOperatingPoint(primaryDataMeta.zero)

  override def createResults(
      state: FixedState,
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
          "Expected primary data of type " +
            s"${implicitly[ClassTag[P]].runtimeClass.getSimpleName}, " +
            s"got $receivedData"
        )
      }

  override def calcFlexOptions(
      state: FixedState,
      relevantData: PrimaryOperationRelevantData[P],
  ): FlexibilityMessage.ProvideFlexOptions = {
    val (operatingPoint, _) = determineOperatingPoint(state, relevantData)

    ProvideMinMaxFlexOptions.noFlexOption(uuid, operatingPoint.activePower)
  }

  override def handlePowerControl(
      state: FixedState,
      relevantData: PrimaryOperationRelevantData[P],
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (PrimaryOperatingPoint[P], OperationChangeIndicator) = {
    val factor = relevantData.data.p / setPower
    val scaledData: P = primaryDataMeta.scale(relevantData.data, factor)

    (PrimaryOperatingPoint(scaledData), OperationChangeIndicator())
  }

}

object PrimaryDataParticipantModel {

  final case class PrimaryOperationRelevantData[+P <: PrimaryData](data: P)
      extends OperationRelevantData

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
