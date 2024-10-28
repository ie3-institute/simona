/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.util.StringUtils
import edu.ie3.util.quantities.PowerSystemUnits.{MEGAWATT, MEGAWATTHOUR}
import squants.energy.{MegawattHours, Megawatts}
import squants.{Energy, Power}

/** Denoting difference referencing scenarios for scaling load model output
  */
sealed trait LoadReference {
  val key: String

  def getKey: String = key

  def scale(factor: Double): LoadReference
}
object LoadReference {

  /** Scale the load model behaviour to reach the given active power in max
    *
    * @param power
    *   Foreseen active power
    */
  final case class ActivePower(power: Power) extends LoadReference {
    override val key: String = "power"

    override def scale(factor: Double): ActivePower =
      copy(power = power * factor)
  }

  /** Scale the load model behaviour to reach the given annual energy
    * consumption
    *
    * @param energyConsumption
    *   Annual energy consumption to reach
    */
  final case class EnergyConsumption(
      energyConsumption: Energy
  ) extends LoadReference {
    override val key: String = "energy"

    override def scale(factor: Double): LoadReference =
      copy(energyConsumption = energyConsumption * factor)
  }

  def isEligibleKey(key: String): Boolean = {
    Set("power", "energy").contains(key)
  }

  /** Build a reference object, that denotes, to which reference a load model
    * behaviour might be scaled. If the behaviour is meant to be scaled to
    * energy consumption and no annual energy consumption is given, an
    * [[IllegalArgumentException]] is thrown
    *
    * @param inputModel
    *   [[LoadInput]] to derive energy information from
    * @param modelConfig
    *   Configuration of model behaviour
    * @return
    *   A [[LoadReference]] for use in [[LoadModel]]
    */
  def apply(
      inputModel: LoadInput,
      modelConfig: LoadRuntimeConfig,
  ): LoadReference =
    StringUtils.cleanString(modelConfig.reference).toLowerCase match {
      case "power" =>
        val activePower = Megawatts(
          inputModel
            .getsRated()
            .to(MEGAWATT)
            .getValue
            .doubleValue
        ) *
          inputModel.getCosPhiRated
        LoadReference.ActivePower(activePower)
      case "energy" =>
        Option(inputModel.geteConsAnnual()) match {
          case Some(consumption) =>
            LoadReference.EnergyConsumption(
              MegawattHours(consumption.to(MEGAWATTHOUR).getValue.doubleValue)
            )
          case None =>
            throw new IllegalArgumentException(
              s"Load model with uuid ${inputModel.getUuid} is meant to be scaled to annual energy consumption, but the energy is not provided."
            )
        }
      case unsupported =>
        throw new IllegalArgumentException(
          s"Load model with uuid ${inputModel.getUuid} is meant to be scaled to unsupported reference '$unsupported'."
        )
    }
}
