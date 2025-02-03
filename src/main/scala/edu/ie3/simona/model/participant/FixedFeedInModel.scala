/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.config.RuntimeConfig.SimpleRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID

/** Fixed feed generation model delivering constant power
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  */
final case class FixedFeedInModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: ApparentPower,
    cosPhiRated: Double,
) extends SystemParticipant[
      FixedRelevantData.type,
      ComplexPower,
      ConstantState.type,
    ](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    )
    with ApparentPowerParticipant[FixedRelevantData.type, ConstantState.type] {

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data. Due to the nature of a fixed feed model,
    *   no further data is required.
    * @return
    *   Active power
    */
  override def calculateActivePower(
      modelState: ConstantState.type,
      data: FixedRelevantData.type = FixedRelevantData,
  ): Power = sRated.toActivePower(cosPhiRated) * -1

  override def determineFlexOptions(
      data: FixedRelevantData.type,
      lastState: ConstantState.type,
  ): ProvideFlexOptions =
    ProvideMinMaxFlexOptions.noFlexOption(
      uuid,
      calculateActivePower(lastState, data),
    )

  override def handleControlledPowerChange(
      data: FixedRelevantData.type,
      lastState: ConstantState.type,
      setPower: Power,
  ): (ConstantState.type, FlexChangeIndicator) =
    (lastState, FlexChangeIndicator())
}

object FixedFeedInModel extends LazyLogging {
  def apply(
      inputModel: FixedFeedInInput,
      modelConfiguration: SimpleRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): FixedFeedInModel = {
    val scaledInput =
      inputModel.copy().scale(modelConfiguration.scaling).build()

    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        scaledInput.getOperationTime,
      )

    // build the fixed feed in model
    val model = FixedFeedInModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      QControl.apply(scaledInput.getqCharacteristics),
      Kilovoltamperes(
        scaledInput.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      scaledInput.getCosPhiRated,
    )
    model.enable()
    model
  }
}
