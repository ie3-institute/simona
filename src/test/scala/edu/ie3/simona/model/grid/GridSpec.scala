/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.linalg.DenseMatrix
import breeze.math.Complex
import breeze.numerics.abs
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.simona.exceptions.GridInconsistencyException
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.test.common.input.{GridInputTestData, LineInputTestData}
import edu.ie3.simona.test.common.model.grid.{
  BasicGrid,
  BasicGridWithSwitches,
  FiveLinesWithNodes
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}

import java.util.UUID

class GridSpec extends UnitSpec with LineInputTestData with DefaultTestData {

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

      // the update function for the admittance matrix
      val _updateAdmittanceMatrix: (
          Int,
          Int,
          Complex,
          Complex,
          Complex,
          DenseMatrix[Complex]
      ) => DenseMatrix[Complex] = { (i, j, yab, yaa, ybb, admittanceMatrix) =>
        admittanceMatrix(i, i) += (yab + yaa)
        admittanceMatrix(j, j) += (yab + ybb)
        admittanceMatrix(i, j) += (yab * -1)
        admittanceMatrix(j, i) += (yab * -1)
        admittanceMatrix
      }

      // method call
      val buildLinesAdmittanceMatrixMethod
          : PrivateMethod[DenseMatrix[Complex]] =
        PrivateMethod[DenseMatrix[Complex]](
          Symbol("buildLinesAdmittanceMatrix")
        )

      // result of method call
      val actualResult: DenseMatrix[Complex] =
        GridModel invokePrivate buildLinesAdmittanceMatrixMethod(
          nodeUuidToIndexMap,
          lines,
          _updateAdmittanceMatrix
        )

      _printAdmittanceMatrixOnMismatch(actualResult, lineAdmittanceMatrix)

      actualResult shouldBe lineAdmittanceMatrix

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
          switches
        )
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
      val adaptedLines: Set[LineModel] = lines - line01

      // get the grid from the raw data
      val gridModel = new GridModel(
        1,
        default400Kva10KvRefSystem,
        GridComponents(
          nodes,
          adaptedLines,
          Set(transformer2wModel),
          Set.empty[Transformer3wModel],
          Set.empty[SwitchModel]
        )
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

    "throw an InvalidGridException if two switches are connected @ the same node" in new BasicGridWithSwitches {
      // enable nodes
      override val nodes: Seq[NodeModel] = super.nodes
      nodes.foreach(_.enable())

      // add a second switch @ node13 (between node1 and node13)
      val secondSwitch = new SwitchModel(
        UUID.fromString("ebeaad04-0ee3-4b2e-ae85-8c76a583295b"),
        "SecondSwitch1",
        defaultOperationInterval,
        node1.uuid,
        node13.uuid
      )
      // add the second switch + enable switches
      override val switches: Set[SwitchModel] = super.switches + secondSwitch
      switches.foreach(_.enable())
      // open the switches
      switches.foreach(_.open())

      // get the grid from the raw data
      val gridModel = new GridModel(
        1,
        default400Kva10KvRefSystem,
        GridComponents(
          nodes,
          lines,
          Set(transformer2wModel),
          Set.empty[Transformer3wModel],
          switches
        )
      )

      // get the private method for validation
      val validateConsistency: PrivateMethod[Unit] =
        PrivateMethod[Unit](Symbol("validateConsistency"))

      // call the validation method
      val exception: InvalidGridException = intercept[InvalidGridException] {
        GridModel invokePrivate validateConsistency(gridModel)
      }

      // expect an exception for node 13
      exception.getMessage shouldBe s"The grid model for subnet 1 has nodes with multiple switches. This is not supported yet! Duplicates are located @ nodes: Vector(${node13.uuid})"

    }

    "update the nodeUuidToIndexMap correctly, when the given grid" should {

      "contains 3 open switches" in new BasicGridWithSwitches {
        // enable nodes
        override val nodes: Seq[NodeModel] = super.nodes
        nodes.foreach(_.enable())
        // enable switches
        override val switches: Set[SwitchModel] = super.switches
        switches.foreach(_.enable())
        // open the switches
        switches.foreach(_.open())

        // get the grid from the raw data
        val gridModel = new GridModel(
          1,
          default400Kva10KvRefSystem,
          GridComponents(
            nodes,
            lines,
            Set(transformer2wModel),
            Set.empty[Transformer3wModel],
            switches
          )
        )

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

      "contains 3 closed switches" in new BasicGridWithSwitches {
        // enable nodes
        override val nodes: Seq[NodeModel] = super.nodes
        nodes.foreach(_.enable())
        // enable switches
        override val switches: Set[SwitchModel] = super.switches
        switches.foreach(_.enable())
        // switches are closed by default

        // get the grid from the raw data
        val gridModel = new GridModel(
          1,
          default400Kva10KvRefSystem,
          GridComponents(
            nodes,
            lines,
            Set(transformer2wModel),
            Set.empty[Transformer3wModel],
            switches
          )
        )

        // update the uuidToIndexMap
        GridModel.updateUuidToIndexMap(gridModel)

        // values should contain duplicates 3 duplicate values -> 3 nodes with the same index
        gridModel.nodeUuidToIndexMap.values.toVector.sorted.size - 3 should be(
          gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted.size
        )

        // value set should be in the range of 0 to 9 (13 nodes with 3 closed switches -> 3 nodes are aggregated
        // = 9 rows and columns in the admittance matrix)
        gridModel.nodeUuidToIndexMap.values.toSet.toVector.sorted should be(
          (for (i <- 0 to 9) yield i).toSet.toVector.sorted
        )

        // keys should be the same as the node uuids we provided
        gridModel.nodeUuidToIndexMap.keySet.toVector.sorted should be(
          nodes.map(node => node.uuid).toVector.sorted
        )

        // we want the same indices for the switch nodes
        gridModel.nodeUuidToIndexMap.getOrElse(
          node13.uuid,
          throw new RuntimeException(
            "Didn't found node13 in nodeUuidToIndexMap!"
          )
        ) should be(
          gridModel.nodeUuidToIndexMap.getOrElse(
            node14.uuid,
            throw new RuntimeException(
              "Didn't found node14 in nodeUuidToIndexMap!"
            )
          )
        )
        gridModel.nodeUuidToIndexMap.getOrElse(
          node15.uuid,
          throw new RuntimeException(
            "Didn't found node15 in nodeUuidToIndexMap!"
          )
        ) should be(
          gridModel.nodeUuidToIndexMap.getOrElse(
            node16.uuid,
            throw new RuntimeException(
              "Didn't found node16 in nodeUuidToIndexMap!"
            )
          )
        )
        gridModel.nodeUuidToIndexMap.getOrElse(
          node17.uuid,
          throw new RuntimeException(
            "Didn't found node17 in nodeUuidToIndexMap!"
          )
        ) should be(
          gridModel.nodeUuidToIndexMap.getOrElse(
            node18.uuid,
            throw new RuntimeException(
              "Didn't found node18 in nodeUuidToIndexMap!"
            )
          )
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
            switches
          )
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
        gridModel.nodeUuidToIndexMap.getOrElse(
          node13.uuid,
          throw new RuntimeException(
            "Didn't found node13 in nodeUuidToIndexMap!"
          )
        ) should be(
          gridModel.nodeUuidToIndexMap.getOrElse(
            node14.uuid,
            throw new RuntimeException(
              "Didn't found node14 in nodeUuidToIndexMap!"
            )
          )
        )

        gridModel.nodeUuidToIndexMap.getOrElse(
          node17.uuid,
          throw new RuntimeException(
            "Didn't found node17 in nodeUuidToIndexMap!"
          )
        ) should be(
          gridModel.nodeUuidToIndexMap.getOrElse(
            node18.uuid,
            throw new RuntimeException(
              "Didn't found node18 in nodeUuidToIndexMap!"
            )
          )
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
            Set.empty[SwitchModel]
          )
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

    }

    "process a valid GridInputModel without an Exception" in new GridInputTestData {
      GridModel(
        validTestGridInputModel,
        gridInputModelTestDataRefSystem,
        defaultSimulationStart,
        defaultSimulationEnd
      )

    }
  }

}
