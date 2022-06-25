/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import scala.jdk.CollectionConverters.CollectionHasAsScala

case object GridAgentFailFast {

  def failFast(
      gridAgentInitData: GridAgentInitData,
      simonaConfig: SimonaConfig
  ): Unit = {

    /** Check if there is InitData for superior or inferior GridGates
      */
    if (
      gridAgentInitData.superiorGridGates.isEmpty && gridAgentInitData.inferiorGridGates.isEmpty
    )
      throw new GridAgentInitializationException(
        s"${gridAgentInitData.subGridContainer.getGridName} has neither superior nor inferior grids! This can either " +
          s"be cause by wrong subnetGate information or invalid parametrization of the simulation!"
      )

    /** Check if there exits voltage measurements for transformerControlGroups
      */
    val measurementUnitInput =
      gridAgentInitData.subGridContainer.getRawGrid.getMeasurementUnits
    val maybeControlConfig: Option[SimonaConfig.Simona.Control] =
      simonaConfig.simona.control
    if (
      maybeControlConfig.isDefined && !measurementUnitInput.asScala.exists(
        input => input.getVMag
      )
    )
      throw new GridAgentInitializationException(
        s"${gridAgentInitData.subGridContainer.getGridName} has a control group with measurement that don't deliver voltage magnitude."
      )
  }
}
