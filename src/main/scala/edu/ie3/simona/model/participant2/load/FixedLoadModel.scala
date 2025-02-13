/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption,
}
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  FixedState,
  ParticipantFixedState,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.time.Days
import squants.Power

import java.util.UUID

class FixedLoadModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val activePower: Power,
) extends LoadModel[FixedState]
    with ParticipantFixedState[ActivePowerOperatingPoint] {

  override def determineOperatingPoint(
      state: FixedState
  ): (ActivePowerOperatingPoint, Option[Long]) =
    (ActivePowerOperatingPoint(activePower), None)

}

object FixedLoadModel {
  def apply(
      input: LoadInput,
      config: LoadRuntimeConfig,
  ): FixedLoadModel = {
    val reference = LoadReference(input, config)

    val activePower: Power = reference match {
      case ActivePower(power) => power
      case EnergyConsumption(energyConsumption) =>
        val duration = Days(365d)
        energyConsumption / duration
    }

    new FixedLoadModel(
      input.getUuid,
      input.getId,
      Kilovoltamperes(
        input.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      input.getCosPhiRated,
      QControl.apply(input.getqCharacteristics),
      activePower,
    )
  }
}
