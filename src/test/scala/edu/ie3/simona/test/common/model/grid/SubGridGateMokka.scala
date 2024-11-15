/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.grid

import java.util.UUID
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.connector.{
  ConnectorPort,
  Transformer2WInput,
  Transformer3WInput,
}
import org.mockito.Mockito._
import org.scalatestplus.mockito.MockitoSugar

/** Hold my cup of coffee and let me mock you some models.
  */
trait SubGridGateMokka extends MockitoSugar {

  /** Mocks a node with its basic needed information
    *
    * @param uuid
    *   Unique identifier of the node
    * @param subnet
    *   Sub net number
    * @return
    *   [[NodeInput]] with these information
    */
  protected def mockNode(uuid: UUID, subnet: Int): NodeInput = {
    val node = mock[NodeInput]
    when(node.getUuid).thenReturn(uuid)
    when(node.getSubnet).thenReturn(subnet)
    node
  }

  /** Mocks a transformer, that only holds information on what nodes are
    * connected
    *
    * @param nodeA
    *   Node at port A
    * @param nodeB
    *   Node at port B
    * @return
    *   [[Transformer2WInput]] with this information
    */
  protected def mockTransformer2w(
      nodeA: NodeInput,
      nodeB: NodeInput,
  ): Transformer2WInput = {
    val transformer = mock[Transformer2WInput]
    when(transformer.getNodeA).thenReturn(nodeA)
    when(transformer.getNodeB).thenReturn(nodeB)
    transformer
  }

  /** Mocks a transformer, that only holds information on what nodes are
    * connected
    *
    * @param nodeA
    *   Node at port A
    * @param nodeASubnet
    *   Subnet of node A
    * @param nodeB
    *   Node at port B
    * @param nodeC
    *   Node at port C
    * @return
    *   [[Transformer2WInput]] with this information
    */
  protected def mockTransformer3w(
      nodeA: NodeInput,
      nodeASubnet: Int,
      nodeB: NodeInput,
      nodeC: NodeInput,
  ): Transformer3WInput = {
    val internalNode = mock[NodeInput]
    when(internalNode.getUuid).thenReturn(UUID.randomUUID())
    when(internalNode.getSubnet).thenReturn(nodeASubnet)

    val transformer = mock[Transformer3WInput]
    when(transformer.getNodeA).thenReturn(nodeA)
    when(transformer.getNodeB).thenReturn(nodeB)
    when(transformer.getNodeC).thenReturn(nodeC)
    when(transformer.getNodeInternal).thenReturn(internalNode)
    transformer
  }

  /** Builds a sub grid gate by mocking the underlying nodes and transformer
    *
    * @param nodeAUuid
    *   UUID of node A
    * @param subGridA
    *   sub grid number of grid A
    * @param nodeBUuud
    *   UUID of nodeB
    * @param subGridB
    *   sub grid number of grid B
    * @return
    *   A sub grid gate with mocked objects
    */
  protected def build2wSubGridGate(
      nodeAUuid: UUID,
      subGridA: Int,
      nodeBUuud: UUID,
      subGridB: Int,
  ): SubGridGate = {
    val nodeA = mockNode(nodeAUuid, subGridA)
    val nodeB = mockNode(nodeBUuud, subGridB)
    val transformer = mockTransformer2w(nodeA, nodeB)
    SubGridGate.fromTransformer2W(transformer)
  }

  /** Builds a sub grid gate by mocking the underlying nodes and transformer
    *
    * @param nodeAUuid
    *   UUID of node A
    * @param subGridA
    *   sub grid number of grid A
    * @param nodeBUuid
    *   UUID of nodeB
    * @param subGridB
    *   sub grid number of grid B
    * @param nodeCUuid
    *   UUID of nodeC
    * @param subGridC
    *   sub grid number of grid C
    * @param inferiorPort
    *   Select which shall be the lower voltage level port
    * @return
    *   A sub grid gate with mocked objects
    */
  protected def build3wSubGridGate(
      nodeAUuid: UUID,
      subGridA: Int,
      nodeBUuid: UUID,
      subGridB: Int,
      nodeCUuid: UUID,
      subGridC: Int,
      inferiorPort: ConnectorPort,
  ): SubGridGate = {
    val nodeA = mockNode(nodeAUuid, subGridA)
    val nodeB = mockNode(nodeBUuid, subGridB)
    val nodeC = mockNode(nodeCUuid, subGridC)
    val transformer = mockTransformer3w(nodeA, subGridA, nodeB, nodeC)
    SubGridGate.fromTransformer3W(transformer, inferiorPort)
  }
}
