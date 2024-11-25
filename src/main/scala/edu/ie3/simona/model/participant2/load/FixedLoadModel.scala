/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption,
}
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  FixedRelevantData,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.time.Days
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID

class FixedLoadModel(
    override val uuid: UUID,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val activePower: Power,
) extends LoadModel[FixedRelevantData.type] {

  override def determineOperatingPoint(
      state: ParticipantModel.FixedState,
      relevantData: ParticipantModel.FixedRelevantData.type,
  ): (ActivePowerOperatingPoint, Option[Long]) =
    (ActivePowerOperatingPoint(activePower), None)

  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): ParticipantModel.FixedRelevantData.type = FixedRelevantData

}

object FixedLoadModel {
  def apply(
      inputModel: LoadInput,
      config: LoadRuntimeConfig,
  ): FixedLoadModel = {
    val reference = LoadReference(inputModel, config)

    val activePower: Power = reference match {
      case ActivePower(power) => power
      case EnergyConsumption(energyConsumption) =>
        val duration = Days(365d)
        energyConsumption / duration
    }

    new FixedLoadModel(
      inputModel.getUuid,
      Kilovoltamperes(
        inputModel.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      inputModel.getCosPhiRated,
      QControl.apply(inputModel.getqCharacteristics),
      activePower,
    )
  }
}
