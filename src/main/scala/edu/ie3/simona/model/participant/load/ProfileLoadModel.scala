/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  AdditionalFactoryData,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.flex.{
  ParticipantFlexModel,
  ParticipantInflexibleEnergyLimitFlexModel,
  ParticipantInflexiblePowerLimitFlexModel,
}
import edu.ie3.simona.model.participant.load.ProfileLoadModel.LoadModelState
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.service.Data.SecondaryData.{
  LoadDataFunction,
  SecondarySeriesData,
}
import edu.ie3.simona.service.ServiceType.LoadProfileService
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.scala.quantities.ApparentPower
import squants.energy.Energy
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.SortedMap
import edu.ie3.datamodel.models.profile.PowerProfileKey

class ProfileLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    val powerProfileKey: PowerProfileKey,
    val referenceScalingFactor: Double,
) extends LoadModel[LoadModelState] {

  override val flexModels: Map[FlexType, ParticipantFlexModel[
    ActivePowerOperatingPoint,
    LoadModelState,
  ]] =
    Map(
      FlexType.PowerLimit -> ParticipantInflexiblePowerLimitFlexModel(this),
      FlexType.EnergyBoundaries -> ParticipantInflexibleEnergyLimitFlexModel(
        this,
        _.toStateSeries,
      ),
    )

  override def determineOperatingPoint(
      state: LoadModelState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    val (_, averagePower) = state.powerData
      .maxBefore(state.tick + 1)
      .getOrElse(
        throw new CriticalFailureException(
          s"No power data available for current tick ${state.tick}"
        )
      )

    (
      ActivePowerOperatingPoint(averagePower * referenceScalingFactor),
      None,
    )
  }

  /** Determines the current state given the last state and the operating point
    * that has been valid from the last state up until now.
    *
    * @param lastState
    *   The last state.
    * @param operatingPoint
    *   The operating point valid from the simulation time of the last state up
    *   until now.
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

  override def handleInput(
      state: LoadModelState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): LoadModelState = {
    receivedData
      .collectFirst {
        case loadFunction: LoadDataFunction =>
          SortedMap(state.tick -> loadFunction.powerSupplier())

        case SecondarySeriesData(series) =>
          series.map {
            case (tick, loadFunction: LoadDataFunction) =>
              tick -> loadFunction.powerSupplier()
            case (_, unexpectedData) =>
              throw new CriticalFailureException(
                s"Unexpected secondary data $unexpectedData"
              )
          }
      }
      .map(newData => state.copy(powerData = newData))
      .getOrElse(state)
  }
}

object ProfileLoadModel {

  /** Holds all relevant data for profile load model calculation
    *
    * @param powerData
    *   A map of tick to corresponding power. For regular calculations of the
    *   operating point, only power for the current tick is used. For forecasts,
    *   the map needs to contain further power values for future ticks.
    */
  final case class LoadModelState(
      override val tick: Long,
      powerData: SortedMap[Long, Power] = SortedMap.empty,
  ) extends ModelState {

    /** Creates states for forecast calculation given the current state.
      *
      * @return
      *   States for forecast calculation.
      */
    def toStateSeries: SortedMap[Long, LoadModelState] = {
      powerData.map { case (dataTick, _) =>
        val tickState = LoadModelState(
          dataTick,
          powerData,
        )

        dataTick -> tickState
      }
    }

  }

  object LoadModelState {

    /** Convenience constructor for creating a state for regular operating point
      * calculation at the current point in simulation time.
      *
      * @param tick
      *   The current tick.
      * @param avgPower
      *   The average power for the given tick.
      * @return
      *   A state for calculation at the current point in simulation time.
      */
    def apply(
        tick: Long,
        avgPower: Power,
    ): LoadModelState =
      LoadModelState(
        tick,
        SortedMap(tick -> avgPower),
      )

  }

  /** Hold additional data for some load model factories.
    * @param maxPower
    *   The maximal power of the
    *   [[edu.ie3.datamodel.models.profile.LoadProfile]].
    * @param energyScaling
    *   The energy scaling for the
    *   [[edu.ie3.datamodel.models.profile.LoadProfile]].
    */
  final case class ProfileLoadFactoryData(
      maxPower: Option[Power],
      energyScaling: Option[Energy],
  ) extends AdditionalFactoryData

  final case class Factory(
      input: LoadInput,
      config: LoadRuntimeConfig,
      maxPower: Option[Power] = None,
      energyScaling: Option[Energy] = None,
  ) extends ParticipantModelFactory[LoadModelState] {

    override def update(
        data: AdditionalFactoryData
    ): Factory = data match {
      case ProfileLoadFactoryData(maxPower, energyScaling) =>
        copy(maxPower = maxPower, energyScaling = energyScaling)

      case unexpected =>
        throw new CriticalFailureException(
          s"Received unexpected data '$unexpected', while updating the profile load model factory."
        )

    }

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(LoadProfileService)

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): LoadModelState = LoadModelState(tick)

    override def create(): ProfileLoadModel = {
      val referenceType = LoadReferenceType(config.reference)

      val power = maxPower.getOrElse(
        throw new SourceException(
          s"Expected a maximal power value for this load profile: ${input.getLoadProfile}!"
        )
      )

      val profileReferenceEnergy = energyScaling.getOrElse(
        throw new SourceException(
          s"Expected a profile energy scaling value for this load profile: ${input.getLoadProfile}!"
        )
      )

      val (referenceScalingFactor, scaledSRated) = LoadModel.scaleToReference(
        referenceType,
        input,
        power,
        profileReferenceEnergy,
      )

      val randomKey = RandomLoadProfile.RANDOM_LOAD_PROFILE.getKey
      val sRated = input.getLoadProfile match {
        case `randomKey` =>
          /** Safety factor to address potential higher sRated values when using
            * unrestricted probability functions.
            */
          scaledSRated * 1.1
        case _ =>
          scaledSRated
      }

      new ProfileLoadModel(
        input.getUuid,
        input.getId,
        sRated,
        input.getCosPhiRated,
        QControl.apply(input.getqCharacteristics()),
        input.getLoadProfile,
        referenceScalingFactor,
      )
    }

  }

}
