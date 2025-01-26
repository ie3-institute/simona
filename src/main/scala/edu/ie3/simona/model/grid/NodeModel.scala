/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.voltagelevels.VoltageLevel
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import squants.Each

import java.time.ZonedDateTime
import java.util.UUID

/** This model represents an electric node
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param isSlack
  *   true, if this node is a slack node
  * @param vTarget
  *   the target voltage of this node in p.u.
  * @param voltLvl
  *   voltage level, in which the node is located
  */
final case class NodeModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    isSlack: Boolean,
    vTarget: squants.Dimensionless,
    voltLvl: VoltageLevel,
) extends SystemComponent(
      uuid,
      id,
      operationInterval,
    )

case object NodeModel {
  def apply(
      nodeInput: NodeInput,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): NodeModel = {

    // validate the input model
    validateInputModel(nodeInput)

    val operationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        nodeInput.getOperationTime,
      )

    val nodeModel = new NodeModel(
      nodeInput.getUuid,
      nodeInput.getId,
      operationInterval,
      nodeInput.isSlack,
      Each(nodeInput.getvTarget.to(PowerSystemUnits.PU).getValue.doubleValue()),
      nodeInput.getVoltLvl,
    )

    /* Checks, if the participant is in operation right from the start */
    if (operationInterval.includes(SimonaConstants.FIRST_TICK_IN_SIMULATION))
      nodeModel.enable()

    nodeModel
  }

  /** Validates a provided [[NodeInput]] in the way that checks are performed
    * from a [[NodeModel]] perspective. This means that only parameters relevant
    * for [[NodeModel]] s are checked to ensure that the provided [[NodeInput]]
    * can be used for the construction of a [[NodeModel]]. All sanity checks and
    * validations should be putted below!
    *
    * @param nodeInput
    *   instance of [[NodeInput]] that should be validated
    */
  def validateInputModel(nodeInput: NodeInput): Unit = {
    if (
      nodeInput.getvTarget() == null || nodeInput
        .getvTarget()
        .getValue
        .doubleValue <= 0 || nodeInput
        .getvTarget()
        .getValue
        .doubleValue > 2
    )
      throw new InvalidGridException(
        s"Invalid vTarget parameter in node ${nodeInput.getUuid}: ${nodeInput.getvTarget()}"
      )

  }

}
