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
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.profile.LoadProfileStore
import edu.ie3.simona.model.participant.load.random.RandomLoadParamStore
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  DateTimeData,
}
import edu.ie3.simona.util.TickUtil
import edu.ie3.util.scala.quantities.ApparentPower
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID

class ProfileLoadModel(
    override val uuid: UUID,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val loadProfileStore: LoadProfileStore,
    private val loadProfile: StandardLoadProfile,
    private val referenceScalingFactor: Double,
) extends LoadModel[DateTimeData] {

  override def determineOperatingPoint(
      state: ParticipantModel.ConstantState.type,
      relevantData: DateTimeData,
  ): (ParticipantModel.ActivePowerOperatingPoint, Option[Long]) = {
    val resolution = RandomLoadParamStore.resolution.getSeconds

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

    val loadProfile = input.getLoadProfile.asInstanceOf[StandardLoadProfile]
    val loadProfileMax = loadProfileStore.maxPower(loadProfile)

    val reference = LoadReference(input, config)

    val referenceScalingFactor =
      reference match {
        case LoadReference.ActivePower(power) =>
          power / loadProfileMax
        case LoadReference.EnergyConsumption(energyConsumption) =>
          energyConsumption / LoadProfileStore.defaultLoadProfileEnergyScaling
      }

    // todo maybe this does not need to be so complicated, referenceScalingFactor is already calculated
    val scaledSRated = reference match {
      case LoadReference.ActivePower(power) =>
        LoadModel.scaleSRatedActivePower(input, power)

      case LoadReference.EnergyConsumption(energyConsumption) =>
        LoadModel.scaleSRatedEnergy(
          input,
          energyConsumption,
          loadProfileMax,
          LoadProfileStore.defaultLoadProfileEnergyScaling,
        )
    }

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
