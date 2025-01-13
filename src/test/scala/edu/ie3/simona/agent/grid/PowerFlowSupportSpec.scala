/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.input.MeasurementUnitInput
import edu.ie3.datamodel.models.input.connector.{
  SwitchInput,
  Transformer2WInput,
  Transformer3WInput,
}
import edu.ie3.datamodel.models.input.container.{
  RawGridElements,
  SubGridContainer,
}
import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.{
  ExchangePower,
  ExchangeVoltage,
}
import edu.ie3.simona.config.SimonaConfig.Simona
import edu.ie3.simona.model.grid.{GridModel, RefSystem}
import edu.ie3.simona.test.common.model.grid.{
  BasicGridWithSwitches,
  DbfsTestGrid,
}
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.TestGridFactory
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.{Logger, LoggerFactory}
import squants.electro.Kilovolts
import squants.energy.Megawatts
import squants.{Dimensionless, Each}
import tech.units.indriya.ComparableQuantity

import java.time.{Duration, ZonedDateTime}
import java.util.UUID
import javax.measure.quantity.Angle
import scala.jdk.CollectionConverters.SetHasAsJava
import scala.language.implicitConversions

/** Tests power flow on a grid with switches on two branches, where depending on
  * whether switches are opened or closed, current flows through one or both of
  * the branches.
  *
  * Please refer to the diagram of the grid in [[BasicGridWithSwitches]] in
  * order to comprehend the expected test results.
  */
class PowerFlowSupportSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with BasicGridWithSwitches
    with PowerFlowSupport
    with GridResultsSupport {

  implicit val log: Logger = LoggerFactory.getLogger(this.getClass)
  val actorRef: ActorRef[GridAgent.Request] =
    TestProbe[GridAgent.Request]("mock_grid_agent").ref

  /** Setting voltage at slack node to 110 kV and introducing a load of 1 MW at
    * node 1
    */
  private val receivedValuesStore = ReceivedValuesStore
    .empty(Map.empty, Map.empty, Vector.empty)
    .copy(
      nodeToReceivedSlackVoltage = Map(
        node6.uuid -> Some(
          ExchangeVoltage(
            node6.uuid,
            Kilovolts(110d),
            Kilovolts(0d),
          )
        )
      ),
      nodeToReceivedPower = Map(
        node1.uuid -> Map(
          actorRef -> Some(
            ExchangePower(
              node1.uuid,
              Megawatts(1d),
              Megavars(0d),
            )
          )
        )
      ),
    )

  val currentTolerance = 1e-3 // 1 mA
  val angleTolerance = 1e-3 // 0.001 deg

  /** We test for angle regardless of direction of the lines here, thus
    * normalize to [0, 180] degrees
    */
  private def normalizeAngle(
      angle: ComparableQuantity[Angle]
  ): ComparableQuantity[Angle] = {
    if (angle.isLessThan(0d.asDegreeGeom))
      angle.add(180.asDegreeGeom)
    else if (angle.isGreaterThan(180d.asDegreeGeom))
      angle.subtract(180.asDegreeGeom)
    else
      angle
  }

  "PowerFlowSupport" when {
    "all switches are closed" must {
      "calculate power flow correctly" in {

        // switches are closed per default
        val gridModel = createGridCopy()

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem,
          )

        operatingPoint.length shouldBe 10 withClue "safety check: 13 nodes minus 3 closed switches"

        val result = newtonRaphsonPF(
          gridModel,
          3,
          operatingPoint,
          slackNodeVoltages,
        )(Vector(1e-12)) match {
          case successResult: ValidNewtonRaphsonPFResult => successResult
          case failure => fail(s"Newton-Raphson failed: $failure")
        }

        val sweepValueStore = SweepValueStore(
          result,
          gridModel.gridComponents.nodes,
          gridModel.nodeUuidToIndexMap,
        )

        val pfResult =
          createResultModels(gridModel, sweepValueStore)(
            ZonedDateTime.now(),
            log,
          )

        // left/top side segments should have similar currents
        val loadLinesLeft =
          Iterable(
            line18To1,
            line0To17,
            line0To15,
            line16To3,
          ).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => loadLinesLeft.contains(lineRes.getInputModel))
          .foreach { lineRes =>
            lineRes.getiAMag() should equalWithTolerance(
              30.4954d.asAmpere,
              currentTolerance,
            )
            lineRes.getiBMag() should equalWithTolerance(
              30.4954d.asAmpere,
              currentTolerance,
            )
            normalizeAngle(lineRes.getiAAng()) should equalWithTolerance(
              179.7095d.asDegreeGeom,
              angleTolerance,
            )
            normalizeAngle(lineRes.getiBAng()) should equalWithTolerance(
              179.7095d.asDegreeGeom,
              angleTolerance,
            )
          }

        // right/bottom side segments should have similar currents
        val loadLinesRight =
          Iterable(
            line1To13,
            line14To2,
            line2To3,
          ).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => loadLinesRight.contains(lineRes.getInputModel))
          .foreach { lineRes =>
            lineRes.getiAMag() should equalWithTolerance(
              27.723d.asAmpere,
              currentTolerance,
            )
            lineRes.getiBMag() should equalWithTolerance(
              27.723d.asAmpere,
              currentTolerance,
            )
            normalizeAngle(lineRes.getiAAng()) should equalWithTolerance(
              179.7095d.asDegreeGeom,
              angleTolerance,
            )
            normalizeAngle(lineRes.getiBAng()) should equalWithTolerance(
              179.7095d.asDegreeGeom,
              angleTolerance,
            )
          }

      }
    }

    "switch s3 on the left/top side is open" must {
      "calculate a load flow on the right side" in {

        val gridModel = createGridCopy()
        gridModel.gridComponents.switches
          .find(_.uuid == switch3.uuid)
          .value
          .open()
        GridModel.updateUuidToIndexMap(gridModel)

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem,
          )

        operatingPoint.length shouldBe 11 withClue "safety check: 13 nodes minus 2 closed switches"

        val result = newtonRaphsonPF(
          gridModel,
          50,
          operatingPoint,
          slackNodeVoltages,
        )(Vector(1e-12)) match {
          case successResult: ValidNewtonRaphsonPFResult => successResult
          case failure => fail(s"Newton-Raphson failed: $failure")
        }

        val pfResult = createResultModels(
          gridModel,
          SweepValueStore(
            result,
            gridModel.gridComponents.nodes,
            gridModel.nodeUuidToIndexMap,
          ),
        )(ZonedDateTime.now(), log)

        // left/top side segments (lines that are adjacent to the open switch) should have no load
        val loadLinesLeft =
          Iterable(line18To1, line0To17, line0To15, line16To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => loadLinesLeft.contains(lineRes.getInputModel))
          .foreach { lineRes =>
            lineRes.getiAMag() should equalWithTolerance(
              0.0001d.asAmpere,
              currentTolerance,
            )
            lineRes.getiBMag() should equalWithTolerance(
              0.0001d.asAmpere,
              currentTolerance,
            )
          // angles are not reliable enough with such small magnitudes
          }

        // right/bottom side segments (lines that are adjacent to the closed switch) should have load
        val loadLinesRight =
          Iterable(line1To13, line14To2, line2To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => loadLinesRight.contains(lineRes.getInputModel))
          .foreach { lineRes =>
            lineRes.getiAMag() should equalWithTolerance(
              58.6017d.asAmpere,
              currentTolerance,
            )
            lineRes.getiBMag() should equalWithTolerance(
              58.6017d.asAmpere,
              currentTolerance,
            )
            normalizeAngle(lineRes.getiAAng()) should equalWithTolerance(
              179.4090d.asDegreeGeom,
              angleTolerance,
            )
            normalizeAngle(lineRes.getiBAng()) should equalWithTolerance(
              179.4090d.asDegreeGeom,
              angleTolerance,
            )
          }

      }
    }

    "switch s1 on the bottom/right side is open" must {
      "calculate a power flow on the left side" in {

        val gridModel = createGridCopy()
        gridModel.gridComponents.switches
          .find(_.uuid == switch1.uuid)
          .value
          .open()
        GridModel.updateUuidToIndexMap(gridModel)

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem,
          )

        operatingPoint.length shouldBe 11 withClue "safety check: 13 nodes minus 2 closed switches"

        val result = newtonRaphsonPF(
          gridModel,
          50,
          operatingPoint,
          slackNodeVoltages,
        )(Vector(1e-12)) match {
          case successResult: ValidNewtonRaphsonPFResult => successResult
          case failure => fail(s"Newton-Raphson failed: $failure")
        }

        val pfResult = createResultModels(
          gridModel,
          SweepValueStore(
            result,
            gridModel.gridComponents.nodes,
            gridModel.nodeUuidToIndexMap,
          ),
        )(ZonedDateTime.now(), log)

        // left/top side segments (lines that are adjacent to the open switch) should have load
        val expectedLoadLines =
          Iterable(line18To1, line0To17, line0To15, line16To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => expectedLoadLines.contains(lineRes.getInputModel))
          .foreach { lineRes =>
            lineRes.getiAMag() should equalWithTolerance(
              58.5343d.asAmpere,
              currentTolerance,
            )
            lineRes.getiBMag() should equalWithTolerance(
              58.5343d.asAmpere,
              currentTolerance,
            )
            normalizeAngle(lineRes.getiAAng()) should equalWithTolerance(
              179.461d.asDegreeGeom,
              angleTolerance,
            )
            normalizeAngle(lineRes.getiBAng()) should equalWithTolerance(
              179.461d.asDegreeGeom,
              angleTolerance,
            )
          }

        // right/bottom side segments (lines that are adjacent to the closed switch) should have no load
        val expectedNoLoadLines =
          Iterable(line1To13, line14To2, line2To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes =>
            expectedNoLoadLines.contains(lineRes.getInputModel)
          )
          .foreach { lineRes =>
            lineRes.getiAMag() should equalWithTolerance(
              0.0001d.asAmpere,
              currentTolerance,
            )
            lineRes.getiBMag() should equalWithTolerance(
              0.0001d.asAmpere,
              currentTolerance,
            )
          // angles are not reliable enough with such small magnitudes
          }

      }
    }
  }

  "PowerFlowSupport" should {
    implicit val tolerance: Dimensionless = Each(1e-12)

    val powerFlowParams = PowerFlowParams(
      1e-5,
      Vector(1e-12),
      50,
      Duration.ofMinutes(30),
      stopOnFailure = true,
    )

    "perform the power flow for the slack grid correctly" in {
      val withMvPowerFlowResults: Map[UUID, Dimensionless] = {
        val (gridModel, receivedValueStore) = TestData.withMv

        val (operationPoint, slackNodeVoltages) = composeOperatingPoint(
          gridModel.gridComponents.nodes,
          gridModel.gridComponents.transformers,
          gridModel.gridComponents.transformers3w,
          gridModel.nodeUuidToIndexMap,
          receivedValueStore,
          gridModel.mainRefSystem,
          targetVoltageFromReceivedData = false,
        )

        val pf = newtonRaphsonPF(
          gridModel,
          50,
          operationPoint,
          slackNodeVoltages,
        )(Vector(1e-12)) match {
          case result: PowerFlowResult.SuccessFullPowerFlowResult =>
            result
          case _: PowerFlowResult.FailedPowerFlowResult =>
            fail()
        }

        val indexMap = gridModel.nodeUuidToIndexMap.map { case (uuid, i) =>
          (i, uuid)
        }
        pf.nodeData.map(n => indexMap(n.index) -> Each(n.voltage.abs)).toMap
      }

      val onlyLvPowerFlowResults: Map[UUID, Dimensionless] = {
        val (gridModel, receivedValueStore) = TestData.onlyLv

        val pf = slackGridPF(
          gridModel,
          receivedValueStore,
          powerFlowParams,
        ) match {
          case result: PowerFlowResult.SuccessFullPowerFlowResult =>
            result
          case _: PowerFlowResult.FailedPowerFlowResult =>
            fail()
        }

        val indexMap = gridModel.nodeUuidToIndexMap.map { case (uuid, i) =>
          (i, uuid)
        }
        pf.nodeData.map(n => indexMap(n.index) -> Each(n.voltage.abs)).toMap

      }

      onlyLvPowerFlowResults.foreach { case (uuid, result) =>
        withMvPowerFlowResults(uuid) should approximate(result)(tolerance)
      }
    }
  }

  object TestData extends DbfsTestGrid with ConfigTestData {
    val time: Simona.Time = simonaConfig.simona.time

    implicit def toZoneDateTime(time: String): ZonedDateTime =
      TimeUtil.withDefaults.toZonedDateTime(time)

    implicit def toGridModel(
        subGridContainer: SubGridContainer
    ): GridModel =
      GridModel(
        subGridContainer,
        RefSystem("2000 MVA", "110 kV"),
        time.startDateTime,
        time.endDateTime,
        simonaConfig,
      )

    val withMv: (GridModel, ReceivedValuesStore) = {
      val gridModel = TestGridFactory.createSubGrid(
        gridName = "centerGrid",
        subgrid = 1,
        rawGridElements = new RawGridElements(
          Set(supNodeA, node1, node2, node3, node4).asJava,
          Set(line1To2, line2To3, line1To4).asJava,
          Set(transformer1).asJava,
          Set.empty[Transformer3WInput].asJava,
          Set.empty[SwitchInput].asJava,
          Set.empty[MeasurementUnitInput].asJava,
        ),
      )

      val receivedValueStore = ReceivedValuesStore.empty(
        Map.empty,
        Map.empty,
        Vector(supNodeA.getUuid),
      )

      (gridModel, receivedValueStore)
    }

    val onlyLv: (GridModel, ReceivedValuesStore) = {
      val updatedNode1 =
        node1.copy().slack(true).vTarget(0.9999984268502677.asPu).build()
      val gridModel = TestGridFactory.createSubGrid(
        gridName = "centerGrid",
        subgrid = 1,
        rawGridElements = new RawGridElements(
          Set(updatedNode1, node2, node3, node4).asJava,
          Set(
            line1To2.copy().nodeA(updatedNode1).build(),
            line2To3,
            line1To4.copy().nodeB(updatedNode1).build(),
          ).asJava,
          Set.empty[Transformer2WInput].asJava,
          Set.empty[Transformer3WInput].asJava,
          Set.empty[SwitchInput].asJava,
          Set.empty[MeasurementUnitInput].asJava,
        ),
      )

      val receivedValueStore = ReceivedValuesStore.empty(
        Map.empty,
        Map.empty,
        Vector.empty,
      )

      (gridModel, receivedValueStore)
    }
  }
}
