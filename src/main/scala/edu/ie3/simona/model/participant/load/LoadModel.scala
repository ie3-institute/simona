/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.{
  ApparentPowerParticipant,
  SystemParticipant
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import squants.{Energy, Power}
import squants.energy.Megawatts

import java.util.UUID

/** Abstract super class of a load model.
  *
  * @tparam D
  *   Type of data, that is needed for model calculation
  */
abstract class LoadModel[D <: LoadRelevantData](
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double
) extends SystemParticipant[D, ApparentPower](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    )
    with ApparentPowerParticipant[D]

case object LoadModel extends LazyLogging {

  /** Scale profile based load models' sRated based on a provided active power
    * value
    *
    * When the load is scaled to the active power value, the models' sRated is
    * multiplied by the ratio of the provided active power value and the active
    * power value of the model (activePowerVal / (input.sRated*input.cosPhi)
    *
    * @param inputModel
    *   the input model instance
    * @param activePower
    *   the active power value sRated should be scaled to
    * @param safetyFactor
    *   a safety factor to address potential higher sRated values than the
    *   original scaling would provide (e.g. when using unrestricted probability
    *   functions)
    * @return
    *   the inputs model sRated scaled to the provided active power
    */
  def scaleSRatedActivePower(
      inputModel: LoadInput,
      activePower: Power,
      safetyFactor: Double = 1d
  ): Power = {
    val sRated = Megawatts(
      inputModel.getsRated
        .to(PowerSystemUnits.MEGAWATT)
        .getValue
        .doubleValue
    )
    val pRated = sRated * inputModel.getCosPhiRated
    val referenceScalingFactor = activePower / pRated
    sRated * referenceScalingFactor * safetyFactor
  }

  /** Scale profile based load model's sRated based on the provided yearly
    * energy consumption
    *
    * When the load is scaled based on the consumed energy per year, the
    * installed sRated capacity is not usable anymore instead, the load's rated
    * apparent power ist scaled on the maximum power occurring in the specified
    * load profile multiplied by the ratio of the annual consumption and the
    * standard load profile scale
    *
    * @param inputModel
    *   the input model instance
    * @param energyConsumption
    *   the yearly energy consumption the models' sRated should be scaled to
    * @param profileMaxPower
    *   the maximum power value of the profile
    * @param profileEnergyScaling
    *   the energy scaling factor of the profile (= amount of yearly energy the
    *   profile is scaled to)
    * @param safetyFactor
    *   a safety factor to address potential higher sRated values than the
    *   original scaling would provide (e.g. when using unrestricted probability
    *   functions)
    * @return
    *   he inputs model sRated scaled to the provided energy consumption
    */
  def scaleSRatedEnergy(
      inputModel: LoadInput,
      energyConsumption: Energy,
      profileMaxPower: Power,
      profileEnergyScaling: Energy,
      safetyFactor: Double = 1d
  ): Power = {
    (profileMaxPower / inputModel.getCosPhiRated) * (
      energyConsumption / profileEnergyScaling
    ) * safetyFactor
  }

}
