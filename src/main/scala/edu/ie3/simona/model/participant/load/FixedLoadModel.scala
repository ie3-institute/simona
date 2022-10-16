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
  EnergyConsumption
}
import edu.ie3.util.quantities.PowerSystemUnits.{MEGAVOLTAMPERE, MEGAWATT}
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{HOUR, YEAR}

import java.util.UUID
import javax.measure.quantity.Power

/** Load model always consuming the same, constant power
  *
  * @param uuid
  *   unique identifier
  * @param id
  *   human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param scalingFactor
  *   Scaling the output of the system
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
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    reference: LoadReference
) extends LoadModel[FixedLoadRelevantData.type](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated.to(MEGAVOLTAMPERE),
      cosPhiRated
    ) {
  private val activePower = reference match {
    case ActivePower(power) => power.to(MEGAWATT)
    case EnergyConsumption(energyConsumption) =>
      val duration = Quantities.getQuantity(1d, YEAR).to(HOUR)
      energyConsumption.divide(duration).asType(classOf[Power]).to(MEGAWATT)
  }

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data. Due to the nature of a fixed load model,
    *   no further data is needed.
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      maybeModelState: Option[ConstantState.type],
      data: FixedLoadRelevantData.type = FixedLoadRelevantData
  ): ComparableQuantity[Power] = activePower.multiply(scalingFactor)
}

object FixedLoadModel {
  case object FixedLoadRelevantData extends LoadRelevantData

  def apply(
      input: LoadInput,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      reference: LoadReference
  ): FixedLoadModel = FixedLoadModel(
    input.getUuid,
    input.getId,
    operationInterval,
    scalingFactor,
    QControl(input.getqCharacteristics()),
    input.getsRated(),
    input.getCosPhiRated,
    reference
  )
}
