/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.input.container.SubGridContainer
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException

import java.util.UUID
import scala.jdk.CollectionConverters._

object GridAgentFailFast {

  /** FailFast Check of GridAgent at Initialisation
    * @param gridAgentInitData
    *   Data that is send to the [[GridAgent]] directly after startup. It
    *   contains the main information for initialization.
    * @param simonaConfig
    *   the config that should be checked
    */
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

    checkControlGroupsForMeasurement(
      gridAgentInitData.subGridContainer,
      simonaConfig.simona.control
    )
  }

  /** Checks all ControlGroups if a) Transformer of ControlGroup and Measurement
    * belongs to the same sub grid. b) Measurements are measure voltage
    * magnitude.
    *
    * @param subGridContainer
    *   Container of all models for this sub grid
    * @param maybeControlConfig
    *   Config of ControlGroup
    */
  def checkControlGroupsForMeasurement(
      subGridContainer: SubGridContainer,
      maybeControlConfig: Option[SimonaConfig.Simona.Control]
  ): Unit = {

    val measurementUnits =
      subGridContainer.getRawGrid.getMeasurementUnits.asScala

    val transformerUnits2W =
      subGridContainer.getRawGrid.getTransformer2Ws.asScala

    val transformerUnits3W =
      subGridContainer.getRawGrid.getTransformer3Ws.asScala

    maybeControlConfig.foreach(control =>
      control.transformer.foreach(controlGroup =>
        controlGroup.transformers.map(UUID.fromString).foreach { transformer =>
          // Check if transformer is part of subgrid of this GridAgent
          val transformerUnit2W = transformerUnits2W
            .find(_.getUuid == transformer)
          val transformerUnit3W = transformerUnits3W
            .find(_.getUuid == transformer)
          if (transformerUnit2W.isDefined || transformerUnit3W.isDefined) {

            controlGroup.measurements
              .map(UUID.fromString)
              .foreach { measurement =>
                val measurementUnit = measurementUnits
                  .find(_.getUuid == measurement)
                  .getOrElse(
                    throw new GridAgentInitializationException(
                      s"${subGridContainer.getGridName} has a transformer control group (${control.transformer.toString}) with a measurement which UUID does not exists in this subnet."
                    )
                  )
                if (!measurementUnit.getVMag)
                  throw new GridAgentInitializationException(
                    s"${subGridContainer.getGridName}  has a transformer control group (${control.transformer.toString}) with a measurement which does not measure voltage magnitude."
                  )
              }
          }
        }
      )
    )
  }
}
