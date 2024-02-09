/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import java.time.ZonedDateTime
import java.util.UUID
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector.SwitchInput
import edu.ie3.simona.exceptions.InvalidActionRequestException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.scala.OperationInterval

import scala.util.{Failure, Success, Try}

/** This model represents an electric power switch
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param nodeAUuid
  *   uuid of node a
  * @param nodeBUuid
  *   uuid of node b
  */
final case class SwitchModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    nodeAUuid: UUID,
    nodeBUuid: UUID,
) extends SystemComponent(
      uuid,
      id,
      operationInterval,
    ) {

  private var _isClosed = true

  /** closes the switch or throws an exception if the switch is already closed
    */
  def close(): Try[String] = {
    if (_isClosed) {
      Failure(
        new InvalidActionRequestException(s"Switch $id is already closed!")
      )
    } else {
      _isClosed = true
      Success(s"Switch $id closed!")
    }
  }

  /** opens the switch or throws an exception if the switch is already opened
    */
  def open(): Try[String] = {
    if (_isClosed) {
      _isClosed = false
      Success(s"Switch $id opened!")
    } else {
      Failure(new InvalidActionRequestException(s"Switch $id is already open!"))
    }
  }

  /** returns the status information if the switch is closed
    * @return
    *   true if the switch is closed
    */
  def isClosed: Boolean = _isClosed

  /** returns the status information if the switch is open
    * @return
    *   true if the switch is open
    */
  def isOpen: Boolean = !_isClosed

}

case object SwitchModel {
  def apply(
      switchInput: SwitchInput,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): SwitchModel = {

    // validate the input model
    validateInputModel(switchInput)

    val operationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        switchInput.getOperationTime,
      )

    val switchModel = new SwitchModel(
      switchInput.getUuid,
      switchInput.getId,
      operationInterval,
      switchInput.getNodeA.getUuid,
      switchInput.getNodeB.getUuid,
    )
    if (!switchInput.isClosed)
      switchModel.open()

    // if the switch input model is in operation, enable the model
    if (operationInterval.includes(SimonaConstants.FIRST_TICK_IN_SIMULATION))
      switchModel.enable()

    switchModel
  }

  /** Validates a provided [[SwitchInput]] in the way that checks are performed
    * from a [[SwitchModel]] perspective. This means that only parameters
    * relevant for [[SwitchModel]] s are checked to ensure that the provided
    * [[SwitchInput]] can be used for the construction of a [[SwitchModel]]. All
    * sanity checks and validations should be putted below!
    *
    * @param switchInput
    *   instance of [[SwitchInput]] that should be validated
    */
  def validateInputModel(switchInput: SwitchInput): Unit = {

    // nodeA == nodeB ?
    if (switchInput.getNodeA.getUuid == switchInput.getNodeB.getUuid)
      throw new InvalidGridException(
        s"Switch ${switchInput.getUuid} has the same nodes on port A and B! " +
          s"NodeA: ${switchInput.getNodeA.getUuid}, NodeB: ${switchInput.getNodeB.getUuid}"
      )

    // nodeA.vRated != nodeB.vRated ?
    if (
      switchInput.getNodeA.getVoltLvl.getNominalVoltage != switchInput.getNodeB.getVoltLvl.getNominalVoltage
    )
      throw new InvalidGridException(
        s"Nodes of switch ${switchInput.getUuid} have different volt levels! " +
          s"vNom: (nodeA: ${switchInput.getNodeA.getVoltLvl.getNominalVoltage}, NodeB: ${switchInput.getNodeB.getVoltLvl.getNominalVoltage})"
      )
  }
}
