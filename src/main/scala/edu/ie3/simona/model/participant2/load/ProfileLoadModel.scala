/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.profile.{LoadProfile, StandardLoadProfile}
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.load.LoadModel.{
  LoadModelState,
  LoadModelWithService,
}
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  DateTimeState,
  ParticipantDateTimeState,
  ParticipantModelFactory,
}
import edu.ie3.simona.service.load.LoadProfileStore
import edu.ie3.simona.model.participant2.load.{LoadModel, LoadReferenceType}
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.util.TickUtil
import edu.ie3.util.scala.quantities.ApparentPower

import java.time.ZonedDateTime
import java.util.UUID

class ProfileLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    val loadProfile: LoadProfile,
    override val referenceScalingFactor: Double,
) extends LoadModel[LoadModelState]
    with LoadModelWithService

object ProfileLoadModel {

  final case class Factory(
      input: LoadInput,
      config: LoadRuntimeConfig,
  ) extends ParticipantModelFactory[DateTimeState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): DateTimeState = DateTimeState(tick, simulationTime)

    override def create(): ProfileLoadModel = {
      val loadProfileStore = LoadProfileStore()

      val loadProfile = input.getLoadProfile match {
        case slp: StandardLoadProfile =>
          slp
        case other =>
          throw new CriticalFailureException(
            s"Expected a standard load profile type, got ${other.getClass}"
          )
      }

      val referenceType = LoadReferenceType(config.reference)

    val maxPower = loadProfileStore
      .maxPower(loadProfile)
      .getOrElse(
        throw new SourceException(
          s"Expected a maximal power value for this load profile: ${input.getLoadProfile}!"
        )
      )

    val profileReferenceEnergy = loadProfileStore.profileScaling(loadProfile)

    val (referenceScalingFactor, scaledSRated) =
      LoadModel.scaleToReference(
        referenceType,
        input,
        maxPower,
        profileReferenceEnergy,
      )

    new ProfileLoadModel(
      input.getUuid,
      input.getId,
      scaledSRated,
      input.getCosPhiRated,
      QControl.apply(input.getqCharacteristics()),
      loadProfile,
      referenceScalingFactor,
    )
  }

  }

}
