/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile
import edu.ie3.datamodel.models.profile.{LoadProfile, StandardLoadProfile}
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  AdditionalFactoryData,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.ProfileLoadModel.LoadModelState
import edu.ie3.simona.service.Data.SecondaryData.{LoadData, LoadDataFunction}
import edu.ie3.simona.service.ServiceType.LoadProfileService
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.energy.Energy
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID

class ProfileLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    val loadProfile: LoadProfile,
    val referenceScalingFactor: Double,
) extends LoadModel[LoadModelState] {

  override def determineOperatingPoint(
      state: LoadModelState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    val averagePower = state.averagePower

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

    val averagePower = receivedData
      .collectFirst {
        case loadData: LoadData =>
          loadData.averagePower

        case loadFunction: LoadDataFunction =>
          loadFunction.powerSupplier()
      }
      .getOrElse(
        throw new CriticalFailureException(
          s"Expected LoadProfileData, got $receivedData"
        )
      )

    state.copy(averagePower = averagePower)
  }
}

object ProfileLoadModel {

  /** Holds all relevant data for profile load model calculation
    *
    * @param averagePower
    *   the average power for the current interval
    */
  final case class LoadModelState(
      override val tick: Long,
      averagePower: Power,
  ) extends ModelState

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
    ): LoadModelState = LoadModelState(tick, zeroKW)

    override def create(): ProfileLoadModel = {
      val loadProfile = input.getLoadProfile match {
        case slp: StandardLoadProfile =>
          slp
        case random: RandomLoadProfile =>
          random
        case other =>
          throw new CriticalFailureException(
            s"Expected a standard load profile type, got ${other.getClass}"
          )
      }

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

      val sRated = loadProfile match {
        case RandomLoadProfile.RANDOM_LOAD_PROFILE =>
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
        loadProfile,
        referenceScalingFactor,
      )
    }

  }

}
