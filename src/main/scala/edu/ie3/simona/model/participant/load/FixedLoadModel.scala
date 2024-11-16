/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.FixedLoadModel.FixedLoadRelevantData
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import squants.Power
import squants.energy.Kilowatts
import squants.time.Days

import java.util.UUID

/** Load model always consuming the same, constant power
  *
  * @param uuid
  *   unique identifier
  * @param id
  *   human-readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  * @param reference
  *   Scale the fixed output to this reference
  */
final case class FixedLoadModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    reference: LoadReference,
) extends LoadModel[FixedLoadRelevantData.type](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    ) {

  val activePower: Power = reference match {
    case ActivePower(power) => power
    case EnergyConsumption(energyConsumption) =>
      val duration = Days(365d)
      energyConsumption / duration
  }

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data. Due to the nature of a fixed load model,
    *   no further data is needed.
    * @return
    *   Active power
    */
  override def calculateActivePower(
      modelState: ConstantState.type,
      data: FixedLoadRelevantData.type = FixedLoadRelevantData,
  ): Power = activePower
}

object FixedLoadModel {
  case object FixedLoadRelevantData extends LoadRelevantData

  def apply(
      input: LoadInput,
      scalingFactor: Double,
      operationInterval: OperationInterval,
      reference: LoadReference,
  ): FixedLoadModel = {

    val scaledInput = input.copy().scale(scalingFactor).build()

    val model = FixedLoadModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      QControl(scaledInput.getqCharacteristics()),
      Kilowatts(
        scaledInput.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      scaledInput.getCosPhiRated,
      reference,
    )
    model.enable()
    model
  }
}
