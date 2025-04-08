/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.result.system.{
  LoadResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  OperatingPoint,
  ParticipantModelFactory,
}
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage.LoadData
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits.{KILOVOLTAMPERE, KILOWATTHOUR}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.energy.KilowattHours
import squants.{Dimensionless, Energy, Power}

import java.time.ZonedDateTime

abstract class LoadModel[S <: ModelState]
    extends ParticipantModel[
      ActivePowerOperatingPoint,
      S,
    ]
    with ParticipantSimpleFlexibility[S] {

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: S,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      new LoadResult(
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
    new LoadResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

}

object LoadModel {

  /** Holds all relevant data for profile load model calculation
    *
    * @param averagePower
    *   the average power for the current interval
    */
  final case class LoadModelState(
      override val tick: Long,
      averagePower: Power,
  ) extends ModelState

  /** Calculates the scaling factor and scaled rated apparent power according to
    * the reference type
    *
    * @param referenceType
    *   The type of reference according to which scaling is calculated
    * @param input
    *   The [[LoadInput]] of the model
    * @param maxPower
    *   The maximum power consumption possible for the model
    * @param referenceEnergy
    *   The (annual) reference energy relevant to the load model
    * @return
    *   the reference scaling factor used for calculation of specific power
    *   consumption values and the scaled rated apparent power
    */
  def scaleToReference(
      referenceType: LoadReferenceType.Value,
      input: LoadInput,
      maxPower: Power,
      referenceEnergy: Energy,
  ): (Double, ApparentPower) = {
    val sRated = Kilovoltamperes(
      input.getsRated
        .to(KILOVOLTAMPERE)
        .getValue
        .doubleValue
    )
    val eConsAnnual = KilowattHours(
      input.geteConsAnnual().to(KILOWATTHOUR).getValue.doubleValue
    )

    val referenceScalingFactor = referenceType match {
      case LoadReferenceType.ACTIVE_POWER =>
        val pRated = sRated.toActivePower(input.getCosPhiRated)
        pRated / maxPower
      case LoadReferenceType.ENERGY_CONSUMPTION =>
        eConsAnnual / referenceEnergy
    }

    val scaledSRated = referenceType match {
      case LoadReferenceType.ACTIVE_POWER =>
        sRated
      case LoadReferenceType.ENERGY_CONSUMPTION =>
        val maxApparentPower = Kilovoltamperes(
          maxPower.toKilowatts / input.getCosPhiRated
        )
        maxApparentPower * referenceScalingFactor
    }

    (referenceScalingFactor, scaledSRated)
  }

  def getFactory(
      input: LoadInput,
      config: LoadRuntimeConfig,
  ): ParticipantModelFactory[_ <: ModelState] =
    LoadModelBehaviour(config.modelBehaviour) match {
      case LoadModelBehaviour.FIX =>
        FixedLoadModel.Factory(input, config)
      case LoadModelBehaviour.PROFILE =>
        ProfileLoadModel.Factory(input, config)
      case LoadModelBehaviour.RANDOM =>
        RandomLoadModel.Factory(input, config)
    }
  }

  /** Some implementations common for load models with secondary service.
    */
  trait LoadModelWithService {
    this: LoadModel[LoadModelState] =>

    val referenceScalingFactor: Double

    /** Determines the initial state given an initial model input.
      */
    override val initialState: (Long, ZonedDateTime) => LoadModelState =
      (tick, _) => LoadModelState(tick, zeroKW)

    override def determineOperatingPoint(
        state: LoadModelState
    ): (ParticipantModel.ActivePowerOperatingPoint, Option[Long]) = {
      val averagePower = state.averagePower

      (
        ActivePowerOperatingPoint(averagePower * referenceScalingFactor),
        None,
      )
    }

    /** Determines the current state given the last state and the operating
      * point that has been valid from the last state up until now.
      *
      * @param lastState
      *   The last state.
      * @param operatingPoint
      *   The operating point valid from the simulation time of the last state
      *   up until now.
      * @param tick
      *   The current tick
      * @param simulationTime
      *   The current simulation time
      * @return
      *   The current state.
      */
    override def determineState(
        lastState: LoadModelState,
        operatingPoint: ActivePowerOperatingPoint,
        tick: Long,
        simulationTime: ZonedDateTime,
    ): LoadModelState = lastState.copy(tick = tick)

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.LoadProfileService)

    override def handleInput(
        state: LoadModelState,
        receivedData: Seq[Data],
        nodalVoltage: Dimensionless,
    ): LoadModelState = {

      val loadData = receivedData
        .collectFirst { case loadData: LoadData =>
          loadData
        }
        .getOrElse(
          throw new CriticalFailureException(
            s"Expected LoadProfileData, got $receivedData"
          )
        )

      state.copy(averagePower = loadData.averagePower)
    }
  }
}
