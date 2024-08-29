/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.linalg.DenseMatrix
import breeze.math.Complex
import breeze.numerics.abs
import edu.ie3.datamodel.models.input.MeasurementUnitInput
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.exceptions.GridInconsistencyException
import edu.ie3.simona.model.control.{GridControls, TransformerControlGroupModel}
import edu.ie3.simona.model.grid.GridModel.{
  GridComponents,
  updateUuidToIndexMap,
}
import edu.ie3.simona.test.common.input.{GridInputTestData, LineInputTestData}
import edu.ie3.simona.test.common.model.grid.{
  BasicGrid,
  BasicGridWithSwitches,
  FiveLinesWithNodes,
}
import edu.ie3.simona.test.common.{ConfigTestData, DefaultTestData, UnitSpec}
import testutils.TestObjectFactory

import java.util.UUID

class GridSpec
    extends UnitSpec
    with LineInputTestData
    with DefaultTestData
    with ConfigTestData {

  private val _printAdmittanceMatrixOnMismatch
      : (DenseMatrix[Complex], DenseMatrix[Complex]) => Unit = {
    (actualMatrix, expectedMatrix) =>
      if (!actualMatrix.equals(expectedMatrix)) {
        for (
          rowIdx <- 0 until expectedMatrix.rows;
          colIdx <- 0 until expectedMatrix.rows
        ) {
          if (
            abs(
              actualMatrix.valueAt(rowIdx, colIdx) - expectedMatrix
                .valueAt(rowIdx, colIdx)
            ) > 1e-12
          )
            logger.debug(
              s"Mismatch in ($rowIdx, $colIdx): Actual = ${actualMatrix
                  .valueAt(rowIdx, colIdx)}, expected = ${expectedMatrix.valueAt(rowIdx, colIdx)}"
            )
        }
      }
  }

  "A GridModel " must {

    "be able to build a valid line admittance matrix for a 6 node 10kV MS grid" in new FiveLinesWithNodes {

      // enable the lines first, otherwise they are not considered in building the admittance matrix
      lines.foreach(_.enable())

      // method call
      val buildAssetAdmittanceMatrix: PrivateMethod[DenseMatrix[Complex]] =
        PrivateMethod[DenseMatrix[Complex]](
          Symbol("buildAssetAdmittanceMatrix")
        )

      val getLinesAdmittanceMethod
          : PrivateMethod[(Int, Int, Complex, Complex, Complex)] =
        PrivateMethod[(Int, Int, Complex, Complex, Complex)](
          Symbol("getLinesAdmittance")
        )
      val getLinesAdmittance: (
          Map[UUID, Int],
          LineModel,
      ) => (Int, Int, Complex, Complex, Complex) =
        (nodeUuidToIndexMap, line) =>
          GridModel invokePrivate getLinesAdmittanceMethod(
            nodeUuidToIndexMap,
            line,
          )

      // result of method call
      val actualResult: DenseMatrix[Complex] =
        GridModel invokePrivate buildAssetAdmittanceMatrix(
          nodeUuidToIndexMap,
          lines,
          getLinesAdmittance,
        )

      _printAdmittanceMatrixOnMismatch(actualResult, lineAdmittanceMatrix)

      actualResult shouldBe lineAdmittanceMatrix

    }

    "be able to build a valid line admittance matrix with switches" in new BasicGridWithSwitches {
      private val withClosedSwitches = createGridCopy()
      closeSwitches(withClosedSwitches)
      GridModel.updateUuidToIndexMap(withClosedSwitches)
      private val admittanceMatixClosed = GridModel.composeAdmittanceMatrix(
        withClosedSwitches.nodeUuidToIndexMap,
        withClosedSwitches.gridComponents,
      )

      private val withOpenSwitches = createGridCopy()
      openSwitches(withOpenSwitches)
      GridModel.updateUuidToIndexMap(withOpenSwitches)
      private val admittanceMatrixOpen = GridModel.composeAdmittanceMatrix(
        withOpenSwitches.nodeUuidToIndexMap,
        withOpenSwitches.gridComponents,
      )

      // dimension of admittance matrix with closed switches should be the dimension
      // of the one with open switches, reduced by the number of closed switches
      private val closedSwitches =
        withClosedSwitches.gridComponents.switches.filter(_.isClosed)
      private val numberClosedSwitches = closedSwitches.size
      numberClosedSwitches shouldBe 3
      admittanceMatixClosed.rows shouldBe admittanceMatrixOpen.rows - numberClosedSwitches
      admittanceMatixClosed.cols shouldBe admittanceMatrixOpen.cols - numberClosedSwitches

      // we're compiling a map of nodes connected by switches, in both directions
      // no transitivity considered (for this test, consecutively connected switches should not be part of the grid!)
      private val switchConnections = closedSwitches.toSeq
        .flatMap { switch =>
          Iterable(
            switch.nodeAUuid -> switch.nodeBUuid,
            switch.nodeBUuid -> switch.nodeAUuid,
          )
        }
        .groupMap { case (key, _) => key } { case (_, value) => value }

      private val nodeUuids = withClosedSwitches.nodeUuidToIndexMap.keys
      // we're going through all node combinations in the closed grid and check whether
      // the admittances in the grid with open switches have been summed up correctly
      nodeUuids.foreach { iNode =>
        // the index of iNode in the grid with closed switches
        val iClosed = withClosedSwitches.nodeUuidToIndexMap.get(iNode).value

        // all indices that have been fused together within the grid with open switches,
        // which are represented only by iNode in the grid with closed switches
        val iOpenAll = switchConnections
          .get(iNode)
          .map(_.map(withOpenSwitches.nodeUuidToIndexMap.get(_).value))
          .getOrElse(Iterable.empty)
          .toSeq :+ withOpenSwitches.nodeUuidToIndexMap.get(iNode).value

        nodeUuids.foreach { jNode =>
          // the index of jNode in the grid with closed switches
          val jClosed = withClosedSwitches.nodeUuidToIndexMap.get(jNode).value

          // all indices that have been fused together within the grid with open switches,
          // which are represented only by jNode in the grid with closed switches
          val jOpenAll = switchConnections
            .get(jNode)
            .map(_.map(withOpenSwitches.nodeUuidToIndexMap.get(_).value))
            .getOrElse(Iterable.empty)
            .toSeq :+ withOpenSwitches.nodeUuidToIndexMap.get(jNode).value

          // all row/column combinations in the grid with open switches are summed up and
          // should then result in the same admittance in the grid with closed switches
          val sumOfAdmittancesOpenSwitches = iOpenAll
            .map { iOpen =>
              jOpenAll
                .map { jOpen =>
                  admittanceMatrixOpen.valueAt(iOpen, jOpen)
                }
                .reduceOption(_ + _)
                .getOrElse(Complex.zero)
            }
            .reduceOption(_ + _)
            .getOrElse(Complex.zero)

          admittanceMatixClosed.valueAt(
            iClosed,
            jClosed,
          ) shouldBe sumOfAdmittancesOpenSwitches withClue s" at \n\tposition ($iClosed, $jClosed) of the grid with closed switches/" +
            s"\n\tpositions (${iOpenAll.mkString(",")}) x (${jOpenAll.mkString(",")}) of the grid with open switches"

        }
      }

    }

    // todo CK
    "be able to build a valid transformer admittance matrix for a" in {}

    "validate the connectivity of a connected grid correctly" in new BasicGridWithSwitches {
      // enable nodes
      override val nodes: Seq[NodeModel] = super.nodes
      nodes.foreach(_.enable())

      // enable lines
      lines.foreach(_.enable())

      // enable transformer
      transformer2wModel.enable()

      // enable switches
      switches.foreach(_.enable())

      // get the grid from the raw data
      val gridModel = new GridModel(
        1,
        default400Kva10KvRefSystem,
        GridComponents(
          nodes,
          lines,
          Set(transformer2wModel),
          Set.empty[Transformer3wModel],
          switches,
        ),
        defaultVoltageLimits,
        GridControls.empty,
      )
      // get the private method for validation
      val validateConnectivity: PrivateMethod[Unit] =
        PrivateMethod[Unit](Symbol("validateConnectivity"))

      GridModel invokePrivate validateConnectivity(gridModel)

    }

    "throw an InvalidGridException if a grid is not connected" in new BasicGridWithSwitches {
      // enable nodes
      override val nodes: Seq[NodeModel] = super.nodes
      nodes.foreach(_.enable())

      // remove a line from the grid
      val adaptedLines = lines - line3To4
      adaptedLines.foreach(_.enable())

      // enable transformer
      transformer2wModel.enable()

      // enable switches
      switches.foreach(_.enable())

      // get the grid from the raw data
      val gridModel = new GridModel(
        1,
        default400Kva10KvRefSystem,
        GridComponents(
          nodes,
          adaptedLines,
          Set(transformer2wModel),
          Set.empty[Transformer3wModel],
          switches,
        ),
        defaultVoltageLimits,
        GridControls.empty,
      )

      // get the private method for validation
      val validateConnectivity: PrivateMethod[Unit] =
        PrivateMethod[Unit](Symbol("validateConnectivity"))

      val exception: GridInconsistencyException =
        intercept[GridInconsistencyException] {
          GridModel invokePrivate validateConnectivity(gridModel)
        }

      exception.getMessage shouldBe "The grid with subnetNo 1 is not connected! Please ensure that all elements are connected correctly and inOperation is set to true!"

    }

    "update the nodeUuidToIndexMap correctly, when the given grid" must {

      "contain 3 open switches" in new BasicGridWithSwitches {

        // get the grid from the raw data
        val gridModel: GridModel = createGridCopy()
        openSwitches(gridModel)

        // update the uuidToIndexMap
        GridModel.updateUuidToIndexMap(gridModel)

        // values should be unique
        gridModel.nodeUuidToIndexMap.values.toVector.sorted should be(
          gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted
        )

        // values should be from 0 to 12 (we have 13 nodes in this grid and all switches
        // are open -> we have 13 rows and columns in the node admittance matrix we need indices for)
        gridModel.nodeUuidToIndexMap.values.toVector.sorted should be(
          (for (i <- 0 to 12) yield i).toVector
        )

        // keys should be the same as the node uuids we provided
        gridModel.nodeUuidToIndexMap.keySet.toVector.sorted should be(
          nodes.map(node => node.uuid).toVector.sorted
        )

      }

      "contain 3 closed switches" in new BasicGridWithSwitches {

        // get the grid from the raw data
        val gridModel: GridModel = createGridCopy()
        closeSwitches(gridModel)

        // update the uuidToIndexMap
        GridModel.updateUuidToIndexMap(gridModel)

        // values should contain duplicates 3 duplicate values -> 3 nodes with the same index
        gridModel.nodeUuidToIndexMap.values.toVector.sorted.size - 3 should be(
          gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted.size
        )

        // value set should be in the range of 0 to 9 (13 nodes with 3 closed switches -> 3 nodes are aggregated
        // = 9 rows and columns in the admittance matrix)
        gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted should be(
          Range.inclusive(0, 9).toVector.sorted
        )

        // keys should be the same as the node uuids we provided
        gridModel.nodeUuidToIndexMap.keySet.toVector.sorted should be(
          nodes.map(node => node.uuid).toVector.sorted
        )

        // we want the same indices for the switch nodes
        gridModel.nodeUuidToIndexMap.get(node13.uuid).value should be(
          gridModel.nodeUuidToIndexMap.get(node14.uuid).value
        )
        gridModel.nodeUuidToIndexMap.get(node15.uuid).value should be(
          gridModel.nodeUuidToIndexMap.get(node16.uuid).value
        )
        gridModel.nodeUuidToIndexMap.get(node17.uuid).value should be(
          gridModel.nodeUuidToIndexMap.get(node18.uuid).value
        )

      }

      "contains 1 open and 2 closed switches" in new BasicGridWithSwitches {
        // enable nodes
        override val nodes: Seq[NodeModel] = super.nodes
        nodes.foreach(_.enable())
        // enable switches
        override val switches: Set[SwitchModel] = super.switches
        switches.foreach(_.enable())
        // switches are closed by default, we open switch2
        switch2.open()

        // get the grid from the raw data
        val gridModel = new GridModel(
          1,
          default400Kva10KvRefSystem,
          GridComponents(
            nodes,
            lines,
            Set(transformer2wModel),
            Set.empty[Transformer3wModel],
            switches,
          ),
          defaultVoltageLimits,
          GridControls.empty,
        )

        // update the uuidToIndexMap
        GridModel.updateUuidToIndexMap(gridModel)

        // values should contain 2 duplicates as two switches are closed
        gridModel.nodeUuidToIndexMap.values.toVector.sorted.size - 2 should be(
          gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted.size
        )

        // values should be from 0 to 10 (we have 13 nodes in this grid and two switches
        // are open -> we have 11 rows and columns in the node admittance matrix we need indices for)
        gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted should be(
          (for (i <- 0 to 10) yield i).toVector
        )

        // keys should be the same as the node uuids we provided
        gridModel.nodeUuidToIndexMap.keySet.toVector.sorted should be(
          nodes.map(node => node.uuid).toVector.sorted
        )

        // we want the same indices for the switch nodes that are closed (switch1 and switch3)
        gridModel.nodeUuidToIndexMap.get(node13.uuid).value should be(
          gridModel.nodeUuidToIndexMap.get(node14.uuid).value
        )
        gridModel.nodeUuidToIndexMap.get(node17.uuid).value should be(
          gridModel.nodeUuidToIndexMap.get(node18.uuid).value
        )

        // different indices for the switch that is open (switch2)
        gridModel.nodeUuidToIndexMap.get(node15.uuid).value shouldNot be(
          gridModel.nodeUuidToIndexMap.get(node16.uuid).value
        )
      }

      "contains no switches" in new BasicGrid {
        // enable nodes
        override val nodes: Seq[NodeModel] = super.nodes
        nodes.foreach(_.enable())

        // get the grid from the raw data
        val gridModel = new GridModel(
          1,
          default400Kva10KvRefSystem,
          GridComponents(
            nodes,
            lines,
            Set(transformer2wModel),
            Set.empty[Transformer3wModel],
            Set.empty[SwitchModel],
          ),
          defaultVoltageLimits,
          GridControls.empty,
        )

        // update the uuidToIndexMap
        GridModel.updateUuidToIndexMap(gridModel)

        // values should be unique
        gridModel.nodeUuidToIndexMap.values.toVector.sorted should be(
          gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted
        )

        // values should be from 0 to 6 (we have 7 nodes in this grid and no switches
        // -> we have 7 rows and columns in the node admittance matrix we need indices for)
        gridModel.nodeUuidToIndexMap.values.toVector.sorted should be(
          (for (i <- 0 to 6) yield i).toVector
        )

        // keys should be the same as the node uuids we provided
        gridModel.nodeUuidToIndexMap.keySet.toVector.sorted should be(
          nodes.map(node => node.uuid).toVector.sorted
        )
      }

      "contains two closed switches with a common node" in new BasicGridWithSwitches {
        // enable nodes
        override val nodes: Seq[NodeModel] = super.nodes
        nodes.foreach(_.enable())

        // add a second switch @ node13 (between node1 and node13)
        val secondSwitch = new SwitchModel(
          UUID.fromString("ebeaad04-0ee3-4b2e-ae85-8c76a583295b"),
          "SecondSwitch1",
          defaultOperationInterval,
          node1.uuid,
          node13.uuid,
        )
        // add the second switch + enable switches
        override val switches: Set[SwitchModel] = super.switches + secondSwitch
        switches.foreach(_.enable())
        // close the switches
        switches.foreach(_.close())

        // get the grid from the raw data
        val gridModel = new GridModel(
          1,
          default400Kva10KvRefSystem,
          GridComponents(
            nodes,
            lines,
            Set(transformer2wModel),
            Set.empty[Transformer3wModel],
            switches,
          ),
          GridControls.empty,
        )

        updateUuidToIndexMap(gridModel)

        // nodes 1, 13 and 14 should map to the same node
        val node1Index = gridModel.nodeUuidToIndexMap
          .get(node1.uuid)
          .value
        gridModel.nodeUuidToIndexMap.get(node13.uuid).value shouldBe node1Index
        gridModel.nodeUuidToIndexMap.get(node14.uuid).value shouldBe node1Index
      }

      "contains closed switches in a complex pattern" in new BasicGridWithSwitches {
        // just six nodes from the basic grid
        override val nodes: Seq[NodeModel] =
          Seq(node1, node2, node3, node4, node5, node6)
        nodes.foreach(_.enable())

        // nodes 1-4 connected by switches in a rectangle plus diagonal,
        // nodes 5-6 connected separately
        override val switches: Set[SwitchModel] = Set(
          SwitchModel(
            UUID.fromString("0-0-0-0-1"),
            "Switch1",
            defaultOperationInterval,
            node1.uuid,
            node2.uuid,
          ),
          SwitchModel(
            UUID.fromString("0-0-0-0-2"),
            "Switch2",
            defaultOperationInterval,
            node2.uuid,
            node3.uuid,
          ),
          SwitchModel(
            UUID.fromString("0-0-0-0-3"),
            "Switch3",
            defaultOperationInterval,
            node3.uuid,
            node4.uuid,
          ),
          SwitchModel(
            UUID.fromString("0-0-0-0-4"),
            "Switch4",
            defaultOperationInterval,
            node1.uuid,
            node4.uuid,
          ),
          SwitchModel(
            UUID.fromString("0-0-0-0-5"),
            "Switch5",
            defaultOperationInterval,
            node2.uuid,
            node4.uuid,
          ),
          SwitchModel(
            UUID.fromString("0-0-0-0-6"),
            "Switch6",
            defaultOperationInterval,
            node5.uuid,
            node6.uuid,
          ),
        )
        switches.foreach(_.enable())
        // close the switches
        switches.foreach(_.close())

        // get the grid from the raw data
        val gridModel = new GridModel(
          1,
          default400Kva10KvRefSystem,
          GridComponents(
            nodes,
            Set.empty,
            Set.empty,
            Set.empty,
            switches,
          ),
          defaultVoltageLimits,
          GridControls.empty,
        )

        // testing the assignment of nodes to indices
        updateUuidToIndexMap(gridModel)

        gridModel.nodeUuidToIndexMap.get(node1.uuid).value shouldBe 0
        gridModel.nodeUuidToIndexMap.get(node2.uuid).value shouldBe 0
        gridModel.nodeUuidToIndexMap.get(node3.uuid).value shouldBe 0
        gridModel.nodeUuidToIndexMap.get(node4.uuid).value shouldBe 0
        gridModel.nodeUuidToIndexMap.get(node5.uuid).value shouldBe 1
        gridModel.nodeUuidToIndexMap.get(node6.uuid).value shouldBe 1
      }

    }

    "build correct transformer control models" should {
      /* Testing of distinct transformer control group building can be found in the spec for transformer control groups */

      "determine node uuids correctly" in {
        val determineNodeUuids =
          PrivateMethod[Set[UUID]](Symbol("determineNodeUuids"))

        val node0 = TestObjectFactory.buildNodeInput(
          false,
          GermanVoltageLevelUtils.MV_10KV,
          1,
        )
        val node1 = TestObjectFactory.buildNodeInput(
          false,
          GermanVoltageLevelUtils.MV_10KV,
          1,
        )
        val node2 = TestObjectFactory.buildNodeInput(
          false,
          GermanVoltageLevelUtils.MV_10KV,
          1,
        )
        val node3 = TestObjectFactory.buildNodeInput(
          false,
          GermanVoltageLevelUtils.MV_10KV,
          1,
        )

        val measurementUnits = Set(
          new MeasurementUnitInput(
            UUID.fromString("3ad9e076-c02b-4cf9-8720-18e2bb541ede"),
            "measurement_unit_0",
            node0,
            true,
            false,
            false,
            false,
          ),
          new MeasurementUnitInput(
            UUID.fromString("ab66fbb0-ece1-44b9-9341-86a884233ec4"),
            "measurement_unit_1",
            node1,
            true,
            false,
            false,
            false,
          ),
          new MeasurementUnitInput(
            UUID.fromString("93b4d0d8-cc67-41f5-9d5c-1cd6dbb2e70d"),
            "measurement_unit_2",
            node2,
            true,
            false,
            false,
            false,
          ),
          new MeasurementUnitInput(
            UUID.fromString("8e84eb8a-2940-4900-b0ce-0eeb6bca8bae"),
            "measurement_unit_3",
            node3,
            false,
            false,
            false,
            false,
          ),
        )
        val selectedMeasurements = Set(
          "ab66fbb0-ece1-44b9-9341-86a884233ec4",
          "93b4d0d8-cc67-41f5-9d5c-1cd6dbb2e70d",
          "8e84eb8a-2940-4900-b0ce-0eeb6bca8bae",
        )
        val expectedUuids = Set(node1, node2).map(_.getUuid)

        val actual =
          TransformerControlGroupModel invokePrivate determineNodeUuids(
            measurementUnits,
            selectedMeasurements,
          )
        actual should contain theSameElementsAs expectedUuids
      }

    }

    "process a valid GridInputModel without an Exception" in new GridInputTestData {
      GridModel(
        validTestGridInputModel,
        gridInputModelTestDataRefSystem,
        defaultVoltageLimits,
        defaultSimulationStart,
        defaultSimulationEnd,
        simonaConfig,
      )

    }
  }

}
