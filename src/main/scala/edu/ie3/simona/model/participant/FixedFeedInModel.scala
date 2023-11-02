/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import squants.Power
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util.UUID

/** Fixed feed generation model delivering constant power
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
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
  */
final case class FixedFeedInModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double
) extends SystemParticipant[
      FixedRelevantData.type,
      ApparentPower,
      ConstantState.type
    ](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
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
  override protected def calculateActivePower(
      modelState: ConstantState.type,
      data: FixedRelevantData.type = FixedRelevantData
  ): Power =
    sRated * (-1) * cosPhiRated * scalingFactor

  override def determineFlexOptions(
      data: FixedRelevantData.type,
      lastState: ConstantState.type
  ): ProvideFlexOptions =
    ProvideFlexOptions.noFlexOption(
      uuid,
      calculateActivePower(ConstantState, data)
    )

  override def handleControlledPowerChange(
      data: FixedRelevantData.type,
      lastState: ConstantState.type,
      setPower: Power
  ): (ConstantState.type, FlexChangeIndicator) =
    (lastState, FlexChangeIndicator())
}

object FixedFeedInModel extends LazyLogging {
  def apply(
      inputModel: FixedFeedInInput,
      modelConfiguration: SimonaConfig.FixedFeedInRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): FixedFeedInModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    // build the fixed feed in model
    val model = FixedFeedInModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      modelConfiguration.scaling,
      QControl.apply(inputModel.getqCharacteristics),
      Kilowatts(
        inputModel.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      inputModel.getCosPhiRated
    )
    model.enable()
    model
  }
}
