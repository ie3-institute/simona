/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.PrimaryDataParticipantModel.*
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.flex.ParticipantFlexModel
import edu.ie3.simona.ontology.messages.flex.{
  FlexOptions,
  FlexType,
  PowerLimitFlexOptions,
}
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  EnrichableData,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.service.Data.{PrimaryData, PrimaryDataExtra}
import edu.ie3.simona.service.{Data, DataTimeType, ServiceType}
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
  * @param scalingFactor
  *   The scaling factor from the runtime config.
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
    private val scalingFactor: Double,
) extends ParticipantModel[
      PrimaryOperatingPoint[PD],
      PrimaryDataState[PD],
    ] {

  override val flexModels: Map[FlexType, ParticipantFlexModel[
    PrimaryOperatingPoint[PD],
    PrimaryDataState[PD],
  ]] = Map(
    FlexType.PowerLimit -> PrimaryDataPowerLimitFlexModel(this)
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
  ): (PrimaryOperatingPoint[PD], Option[Long]) = {
    val scaledData = primaryDataExtra.scale(state.data, scalingFactor)
    (PrimaryOperatingPoint(scaledData), None)
  }

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
      case primaryDataWithApparentPower: PrimaryDataWithComplexPower[?] =>
        primaryDataWithApparentPower
      case enrichableData: EnrichableData[?] =>
        enrichableData.add(complexPower.q)
    }
    Iterable(
      primaryDataResultFunc.createResult(primaryDataWithApparentPower, dateTime)
    )
  }

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[?],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult = throw new CriticalFailureException(
    "Method not implemented by this model."
  )

  override def determineOperatingPoint(
      state: PrimaryDataState[PD],
      setPower: Power,
  ): PrimaryOperatingPoint[PD] = {
    // scale the whole primary data by the same factor that
    // the active power set point was scaled by
    val factor = if setPower.value != 0.0 then {
      state.data.p / setPower
    } else 1.0

    val scaledData: PD = primaryDataExtra.scale(state.data, factor)

    PrimaryOperatingPoint(scaledData)
  }

}

object PrimaryDataParticipantModel {

  /** Trait that provides functionality that can create the same result objects
    * as the corresponding physical object.
    *
    * The function needs to be packaged in a trait in order to be stored in a
    * val.
    */
  private[participant] trait PrimaryResultFunc {
    def createResult(
        data: PrimaryDataWithComplexPower[?],
        dateTime: ZonedDateTime,
    ): SystemParticipantResult
  }

  final case class PrimaryDataState[+PD <: PrimaryData](
      data: PD,
      override val tick: Long,
  ) extends ModelState

  trait PrimaryOperatingPoint[+PD <: PrimaryData] extends OperatingPoint {
    val data: PD

    override val activePower: Power = data.p
  }

  private object PrimaryOperatingPoint {
    def apply[PD <: PrimaryData](
        data: PD
    ): PrimaryOperatingPoint[PD] =
      data match {
        case apparentPowerData: (PD & PrimaryDataWithComplexPower[?]) =>
          PrimaryApparentPowerOperatingPoint(apparentPowerData)
        case other: (PD & EnrichableData[?]) =>
          PrimaryActivePowerOperatingPoint(other)
      }
  }

  private final case class PrimaryApparentPowerOperatingPoint[
      PD <: PrimaryDataWithComplexPower[?]
  ](override val data: PD)
      extends PrimaryOperatingPoint[PD] {
    override val reactivePower: Option[ReactivePower] = Some(data.q)
  }

  private final case class PrimaryActivePowerOperatingPoint[
      PE <: PrimaryData & EnrichableData[? <: PrimaryData]
  ](
      override val data: PE
  ) extends PrimaryOperatingPoint[PE] {
    override val reactivePower: Option[ReactivePower] = None
  }

  /** Flex model for primary data. Does not allow for any flexibility.
    *
    * @param model
    *   The model.
    * @tparam PD
    *   The type of primary data.
    */
  private final case class PrimaryDataPowerLimitFlexModel[PD <: PrimaryData](
      model: PrimaryDataParticipantModel[PD]
  ) extends ParticipantFlexModel[
        PrimaryOperatingPoint[PD],
        PrimaryDataState[PD],
      ] {

    override def determineFlexOptions(
        state: PrimaryDataState[PD],
        dataTimeType: DataTimeType,
    ): FlexOptions = {
      val (operatingPoint, _) = model.determineOperatingPoint(state)

      PowerLimitFlexOptions.noFlexOption(operatingPoint.activePower)
    }

    override def determineNextActivation(
        state: PrimaryDataState[PD],
        operatingPoint: PrimaryOperatingPoint[PD],
        setPower: Power,
        dataTimeType: DataTimeType,
    ): OperationChangeIndicator =
      OperationChangeIndicator.empty

  }

  /** Constructs a [[PrimaryDataParticipantModel]] for the given physical
    * [[ParticipantModel]] and the given primary data.
    *
    * @param physicalModel
    *   The physical participant model.
    * @param primaryDataExtra
    *   Extra functionality specific to the primary data class.
    * @param scalingFactor
    *   The scaling factor from the runtime config.
    */
  final case class Factory[PD <: PrimaryData](
      physicalModel: ParticipantModel[?, ?],
      primaryDataExtra: PrimaryDataExtra[PD],
      scalingFactor: Double,
  ) extends ParticipantModelFactory[PrimaryDataState[PD]] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): PrimaryDataState[PD] =
      PrimaryDataState(
        primaryDataExtra.zero,
        tick,
      )

    override def create(): PrimaryDataParticipantModel[PD] = {
      val primaryResultFunc = new PrimaryResultFunc {
        override def createResult(
            data: PrimaryData.PrimaryDataWithComplexPower[?],
            dateTime: ZonedDateTime,
        ): SystemParticipantResult =
          physicalModel.createPrimaryDataResult(data, dateTime)
      }

      new PrimaryDataParticipantModel(
        physicalModel.uuid,
        physicalModel.id,
        physicalModel.sRated,
        physicalModel.cosPhiRated,
        physicalModel.qControl,
        primaryResultFunc,
        primaryDataExtra,
        scalingFactor,
      )(using primaryDataExtra.getClassTag)
    }
  }

}
