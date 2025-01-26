/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.datamodel.models.input.{MeasurementUnitInput, NodeInput}
import edu.ie3.datamodel.models.input.connector.{
  LineInput,
  SwitchInput,
  Transformer2WInput,
  Transformer3WInput,
}
import edu.ie3.datamodel.models.input.container.{
  GraphicElements,
  JointGridContainer,
  RawGridElements,
  SubGridContainer,
  SystemParticipants,
}
import edu.ie3.datamodel.models.input.graphics.{
  LineGraphicInput,
  NodeGraphicInput,
}
import edu.ie3.datamodel.models.input.system._

import scala.jdk.CollectionConverters._

object TestGridFactory {

  /** Creates a joint grid container for testing purposes.
    *
    * Parameters that are not specified are filled with default values. For
    * container parameters like rawGridElements, and empty container is provided
    * then.
    *
    * @param gridName
    *   The name of the grid, defaults to "TestGrid"
    * @param rawGridElements
    *   The raw grid elements, default to empty container
    * @param systemParticipants
    *   The system participants, default to empty container
    * @param graphicElements
    *   The graphic elements, default to empty container
    * @return
    *   A JointGridContainer for testing purposes
    */
  def createJointGrid(
      gridName: String = "TestGrid",
      rawGridElements: RawGridElements = createEmptyRawGridElements(),
      systemParticipants: SystemParticipants = createEmptySystemParticipants(),
      graphicElements: GraphicElements = createEmptyGraphicElements(),
  ): JointGridContainer =
    new JointGridContainer(
      gridName,
      rawGridElements,
      systemParticipants,
      graphicElements,
    )

  /** Creates a sub grid container for testing purposes.
    *
    * Parameters that are not specified are filled with default values. For
    * container parameters like rawGridElements, and empty container is provided
    * then.
    *
    * @param gridName
    *   The name of the grid, defaults to "TestGrid"
    * @param subgrid
    *   The sub grid number, defaults to 100
    * @param rawGridElements
    *   The raw grid elements, default to empty container
    * @param systemParticipants
    *   The system participants, default to empty container
    * @param graphicElements
    *   The graphic elements, default to empty container
    * @return
    *   A SubGridContainer for testing purposes
    */
  def createSubGrid(
      gridName: String = "TestGrid",
      subgrid: Int = 100,
      rawGridElements: RawGridElements = createEmptyRawGridElements(),
      systemParticipants: SystemParticipants = createEmptySystemParticipants(),
      graphicElements: GraphicElements = createEmptyGraphicElements(),
  ): SubGridContainer =
    new SubGridContainer(
      gridName,
      subgrid,
      rawGridElements,
      systemParticipants,
      graphicElements,
    )

  def createEmptyRawGridElements(): RawGridElements =
    new RawGridElements(
      Set.empty[NodeInput].asJava,
      Set.empty[LineInput].asJava,
      Set.empty[Transformer2WInput].asJava,
      Set.empty[Transformer3WInput].asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava,
    )

  def createEmptySystemParticipants(): SystemParticipants =
    new SystemParticipants(
      Set.empty[BmInput].asJava,
      Set.empty[ChpInput].asJava,
      Set.empty[EvcsInput].asJava,
      Set.empty[EvInput].asJava,
      Set.empty[FixedFeedInInput].asJava,
      Set.empty[HpInput].asJava,
      Set.empty[LoadInput].asJava,
      Set.empty[PvInput].asJava,
      Set.empty[StorageInput].asJava,
      Set.empty[WecInput].asJava,
    )

  def createEmptyGraphicElements(): GraphicElements =
    new GraphicElements(
      Set.empty[NodeGraphicInput].asJava,
      Set.empty[LineGraphicInput].asJava,
    )
}
