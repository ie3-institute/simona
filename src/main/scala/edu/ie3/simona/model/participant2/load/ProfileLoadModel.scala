/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.profile.StandardLoadProfile
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.profile.LoadProfileStore
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  DateTimeData,
}
import edu.ie3.simona.util.TickUtil
import edu.ie3.util.scala.quantities.ApparentPower
import squants.Dimensionless

import java.time.ZonedDateTime
import java.util.UUID

class ProfileLoadModel(
    override val uuid: UUID,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val loadProfileStore: LoadProfileStore,
    private val loadProfile: StandardLoadProfile,
    val referenceScalingFactor: Double,
) extends LoadModel[DateTimeData] {

  override def determineOperatingPoint(
      state: ParticipantModel.FixedState,
      relevantData: DateTimeData,
  ): (ParticipantModel.ActivePowerOperatingPoint, Option[Long]) = {
    val resolution = LoadProfileStore.resolution.getSeconds

    val (modelTick, modelDateTime) = TickUtil.roundToResolution(
      relevantData.tick,
      relevantData.dateTime,
      resolution.toInt,
    )

    val averagePower = loadProfileStore.entry(modelDateTime, loadProfile)
    val nextTick = modelTick + resolution

    (
      ActivePowerOperatingPoint(averagePower * referenceScalingFactor),
      Some(nextTick),
    )
  }

  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): DateTimeData = DateTimeData(tick, simulationTime)

}

object ProfileLoadModel {

  def apply(input: LoadInput, config: LoadRuntimeConfig): ProfileLoadModel = {

    val loadProfileStore: LoadProfileStore = LoadProfileStore()

    val loadProfile = input.getLoadProfile match {
      case slp: StandardLoadProfile =>
        slp
      case other =>
        throw new CriticalFailureException(
          s"Expected a standard load profile type, got ${other.getClass}"
        )
    }

    val referenceType = LoadReferenceType(config)

    val (referenceScalingFactor, scaledSRated) =
      LoadModel.scaleToReference(
        referenceType,
        input,
        loadProfileStore.maxPower(loadProfile),
        LoadProfileStore.profileReferenceEnergy,
      )

    new ProfileLoadModel(
      input.getUuid,
      scaledSRated,
      input.getCosPhiRated,
      QControl.apply(input.getqCharacteristics()),
      loadProfileStore,
      loadProfile,
      referenceScalingFactor,
    )
  }

}
