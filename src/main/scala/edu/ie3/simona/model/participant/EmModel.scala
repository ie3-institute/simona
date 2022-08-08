/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import akka.actor.ActorRef
import edu.ie3.datamodel.models.input.system.{EmInput, SystemParticipantInput}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueFlexibilityControl,
  ProvideFlexibilityOptions
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Power

final case class EmModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    uncontrolledAgents: Seq[(ActorRef, SystemParticipantInput)],
    controlledAgents: Seq[(ActorRef, SystemParticipantInput)]
) extends SystemParticipant[EmRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      Quantities.getQuantity(0, PowerSystemUnits.KILOWATT), // FIXME dummy
      0 // FIXME dummy
    ) {

  /** TODO maybe use UUIDs instead of ActorRefs here, since EmModel is not
    * supposed to send msgs itself
    *
    * Determine the power of controllable devices such as storages
    * @return
    */
  def determineDeviceControl(
      flexOptions: Map[ActorRef, Option[ProvideFlexibilityOptions]]
  ): Seq[(ActorRef, IssueFlexibilityControl)] = ???

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  protected def calculateActivePower(
      data: EmRelevantData
  ): ComparableQuantity[Power] = ???
}

object EmModel {

  /** Class that holds all relevant data for Energy Management calculation
    *
    * @param dateTime
    *   date and time of the <b>ending</b> of time frame to calculate
    */
  final case class EmRelevantData(
      // TODO: From PvModel, Check and refactor
      dateTime: ZonedDateTime,
      lastResults: ValueStore[ApparentPowerAndHeat]
  ) extends CalcRelevantData

  def apply(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      uncontrolledAgents: Seq[(ActorRef, SystemParticipantInput)],
      controlledAgents: Seq[(ActorRef, SystemParticipantInput)]
  ): EmModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    EmModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      modelConfig.scaling,
      QControl(inputModel.getqCharacteristics),
      uncontrolledAgents,
      controlledAgents
    )
  }

}
