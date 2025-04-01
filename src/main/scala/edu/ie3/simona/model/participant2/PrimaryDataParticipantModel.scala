/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.{
  PrimaryData,
  PrimaryDataExtra,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  EnrichableData,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel._
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.util.scala.quantities.{ApparentPower, ReactivePower}
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.ClassTag

/** A [[ParticipantModel]] that does not do any physical calculations, but just
  * "replays" the primary data that it received via model input. It is used in
  * place of a physical [[ParticipantModel]] and thus needs to produce the same
  * type of results.
  *
  * @param primaryDataResultFunc
  *   Function that can create the typical result objects produced by the
  *   physical [[ParticipantModel]].
  * @param primaryDataExtra
  *   Extra functionality specific to the primary data class.
  * @tparam PD
  *   The type of primary data.
  */
final case class PrimaryDataParticipantModel[PD <: PrimaryData: ClassTag](
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val primaryDataResultFunc: PrimaryResultFunc,
    private val primaryDataExtra: PrimaryDataExtra[PD],
) extends ParticipantModel[
      PrimaryOperatingPoint[PD],
      PrimaryDataState[PD],
    ] {

  override val initialState: (Long, ZonedDateTime) => PrimaryDataState[PD] =
    (tick, _) =>
      PrimaryDataState(
        primaryDataExtra.zero,
        tick,
      )

  override def determineState(
      lastState: PrimaryDataState[PD],
      operatingPoint: PrimaryOperatingPoint[PD],
      tick: Long,
      simulationTime: ZonedDateTime,
  ): PrimaryDataState[PD] = lastState.copy(tick = tick)

  override def handleInput(
      state: PrimaryDataState[PD],
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): PrimaryDataState[PD] =
    receivedData
      .collectFirst { case data: PD =>
        data
      }
      .map(newData => state.copy(data = newData))
      .getOrElse(state)

  override def determineOperatingPoint(
      state: PrimaryDataState[PD]
  ): (PrimaryOperatingPoint[PD], Option[Long]) =
    (PrimaryOperatingPoint(state.data), None)

  override def zeroPowerOperatingPoint: PrimaryOperatingPoint[PD] =
    PrimaryOperatingPoint(primaryDataExtra.zero)

  override def createResults(
      state: PrimaryDataState[PD],
      lastOperatingPoint: Option[PrimaryOperatingPoint[PD]],
      currentOperatingPoint: PrimaryOperatingPoint[PD],
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

  override def determineFlexOptions(
      state: PrimaryDataState[PD]
  ): FlexOptions = {
    val (operatingPoint, _) = determineOperatingPoint(state)

    MinMaxFlexOptions.noFlexOption(operatingPoint.activePower)
  }

  override def determineOperatingPoint(
      state: PrimaryDataState[PD],
      setPower: Power,
  ): (PrimaryOperatingPoint[PD], OperationChangeIndicator) = {
    // scale the whole primary data by the same factor that
    // the active power set point was scaled by
    val factor = state.data.p / setPower
    val scaledData: PD = primaryDataExtra.scale(state.data, factor)

    (PrimaryOperatingPoint(scaledData), OperationChangeIndicator())
  }

}

object PrimaryDataParticipantModel {

  final case class PrimaryDataState[+PD <: PrimaryData](
      data: PD,
      override val tick: Long,
  ) extends ModelState

  trait PrimaryOperatingPoint[+PD <: PrimaryData] extends OperatingPoint {
    val data: PD

    override val activePower: Power = data.p
  }

  private object PrimaryOperatingPoint {
    def apply[PD <: PrimaryData: ClassTag](
        data: PD
    ): PrimaryOperatingPoint[PD] =
      data match {
        case apparentPowerData: PD with PrimaryDataWithComplexPower[_] =>
          PrimaryApparentPowerOperatingPoint(apparentPowerData)
        case other: PD with EnrichableData[_] =>
          PrimaryActivePowerOperatingPoint(other)
      }
  }

  private final case class PrimaryApparentPowerOperatingPoint[
      PD <: PrimaryDataWithComplexPower[_]
  ](override val data: PD)
      extends PrimaryOperatingPoint[PD] {
    override val reactivePower: Option[ReactivePower] = Some(data.q)
  }

  private final case class PrimaryActivePowerOperatingPoint[
      PE <: PrimaryData with EnrichableData[_]: ClassTag
  ](
      override val data: PE
  ) extends PrimaryOperatingPoint[PE] {
    override val reactivePower: Option[ReactivePower] = None
  }

  /** Trait that provides functionality that can create the same result objects
    * as the corresponding physical object.
    *
    * The function needs to be packaged in a trait in order to be stored in a
    * val.
    */
  trait PrimaryResultFunc {
    def createResult(
        data: PrimaryDataWithComplexPower[_],
        dateTime: ZonedDateTime,
    ): SystemParticipantResult
  }

}
