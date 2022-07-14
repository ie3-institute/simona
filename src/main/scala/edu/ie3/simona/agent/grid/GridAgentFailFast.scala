/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException

import java.util.UUID
import scala.jdk.CollectionConverters._

object GridAgentFailFast {

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

    val maybeControlConfig: Option[SimonaConfig.Simona.Control] =
      simonaConfig.simona.control

    val measurementUnits =
      gridAgentInitData.subGridContainer.getRawGrid.getMeasurementUnits.asScala

    val transformerUnits2W =
      gridAgentInitData.subGridContainer.getRawGrid.getTransformer2Ws.asScala

    val transformerUnits3W =
      gridAgentInitData.subGridContainer.getRawGrid.getTransformer3Ws.asScala

    maybeControlConfig.foreach(control =>
        control.transformer.foreach(controlGroup =>
          controlGroup.transformers.map(UUID.fromString).foreach {
            transformer =>
              // Check if transformer is part of subgrid of this GridAgent
              val transformerUnit2W = transformerUnits2W
                .find(_.getUuid == transformer)
              val transformerUnit3W = transformerUnits3W
                .find(element => element.getUuid == transformer)
              if (transformerUnit2W.isDefined || transformerUnit3W.isDefined) {

                controlGroup.measurements
                  .map(UUID.fromString)
                  .foreach { measurements =>
                    val measurementUnit = measurementUnits
                      .find(element => element.getUuid == measurements)
                      .getOrElse(
                        throw new GridAgentInitializationException(
                          s"${gridAgentInitData.subGridContainer.getGridName} has a transformer control group (${control.transformer.toString}) with a measurement which UUID does not exists in this subnet."
                        )
                      )
                    if (!measurementUnit.getVMag)
                      throw new GridAgentInitializationException(
                        s"${gridAgentInitData.subGridContainer.getGridName}  has a transformer control group (${control.transformer.toString}) with a measurement which does not measure voltage magnitude."
                      )
                  }
              }
          }
        )
      )

  }
}
