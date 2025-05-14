/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector.Transformer3WInput

sealed trait Transformer3wPowerFlowCase

/** intended to provide an information where the [[Transformer3wModel]] under
  * investigation is located. as we currently duplicate [[Transformer3wModel]]
  * for all child grids, we need to know where the transformer is located to
  * determine the correct electrical parameters. this is done by using the
  * powerflowCase object. Syntax is:
  *
  * [[edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.PowerFlowCaseA]] =>
  * highest voltage side of the transformer is located in this grid
  * [[edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.PowerFlowCaseB]] =>
  * medium voltage side of the transformer is located in this grid
  * [[edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.PowerFlowCaseC]] =>
  * low voltage side of the transformer is located in this grid
  */
object Transformer3wPowerFlowCase {

  case object PowerFlowCaseA extends Transformer3wPowerFlowCase

  case object PowerFlowCaseB extends Transformer3wPowerFlowCase

  case object PowerFlowCaseC extends Transformer3wPowerFlowCase

  def apply(
      trafo3wInput: Transformer3WInput,
      subnetNo: Int,
  ): Transformer3wPowerFlowCase = {
    if trafo3wInput.getNodeA.getSubnet == subnetNo then PowerFlowCaseA
    else if trafo3wInput.getNodeB.getSubnet == subnetNo then PowerFlowCaseB
    else if trafo3wInput.getNodeC.getSubnet == subnetNo then PowerFlowCaseC
    else
      throw new InvalidGridException(
        s"Requested transformer ${trafo3wInput.getUuid} has no node in subnet $subnetNo. Cannot determine the power flow calculation case!"
      )
  }

}
