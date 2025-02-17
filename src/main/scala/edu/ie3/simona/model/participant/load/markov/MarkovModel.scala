/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.markov

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference
import edu.ie3.simona.model.participant.load.markov.MarkovParamStore._
import edu.ie3.simona.model.participant.{CalcRelevantData, FlexChangeIndicator, ModelState, SystemParticipant}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import squants.Dimensionless
import squants.energy.{Kilowatts, Power}

import java.util.UUID

final case class MarkovModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
) extends SystemParticipant[
      MarkovRelevantData,
      ApparentPower,
      ConstantState.type,
    ](
      uuid = ???,
      id = ???,
      operationInterval = ???,
      qControl = ???,
      sRated = ???,
      cosPhiRated = ???,
    ) {

  /** Calculate the power behaviour based on the given data.
    *
    * @param tick
    *   Regarded instant in simulation
    * @param voltage
    *   Nodal voltage magnitude
    * @param modelState
    *   Current state of the model
    * @param data
    *   Further needed, secondary data
    * @return
    *   A tuple of active and reactive power
    */
  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      modelState: ModelState.ConstantState.type,
      data: MarkovRelevantData,
  ): ApparentPower = {
    val activePower = calculateActivePower(modelState, data)
    val reactivePower = calculateReactivePower(activePower, voltage)

    ApparentPower(activePower, reactivePower)
  }

  /** Calculate the active power behaviour of the model
    *
    * @param modelState
    *   Current state of the model
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      modelState: ModelState.ConstantState.type,
      data: MarkovRelevantData,
  ): Power = {

    // Map's

    Kilowatts(1) // Test
  }

  /** @param data
    *   The relevant data for calculation
    * @param lastState
    *   The last reached state
    * @return
    *   flex options
    */
  override def determineFlexOptions(
      data: MarkovRelevantData,
      lastState: ModelState.ConstantState.type,
  ): FlexibilityMessage.ProvideFlexOptions = ???

  /** @param data
    *   The relevant data for calculation
    * @param lastState
    *   The last reached state
    * @param setPower
    *   power that has been set by ???
    * @return
    *   updated relevant data and an indication at which circumstances flex
    *   options will change next
    */
  override def handleControlledPowerChange(
      data: MarkovRelevantData,
      lastState: ModelState.ConstantState.type,
      setPower: Power,
  ): (ModelState.ConstantState.type, FlexChangeIndicator) = ???
}

class MarkovRelevantData extends CalcRelevantData {

  val Usage_Probabilities_Map = Usage_Probabilities()
  val sop_Dish_Washer_Map = sop_Dish_Washer()

  val average_Hh_Map = Average_HH()
  val by_Income_Map = income()
  val by_Inhabitants_Map = inhabitants()
  val by_Type_Map = Type()
  val load_Ts_Map = load_TS()
}


object MarkovModel {
  case object MarkovRelevantData extends LoadRelevantData

  def apply(
    input: LoadInput,
    scalingFactor: Double,
    operationInterval: OperationInterval,
    reference: LoadReference,
  ): MarkovModel = {

    val scaledInput = input.copy().scale(scalingFactor).build()

    val model = MarkovModel(
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
      scaledInput.getCosPhiRated
    )
    model.enable()
    model
  }

}
