/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import breeze.math.Complex
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
}
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.enums.NodeType
import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseA,
  PowerFlowCaseB,
  PowerFlowCaseC,
}
import edu.ie3.simona.model.grid.{
  RefSystem,
  Transformer3wModel,
  TransformerModel,
  TransformerTappingModel,
}
import edu.ie3.simona.test.common.exceptions.InvalidTestDataException
import edu.ie3.simona.test.common.input.GridInputTestData
import edu.ie3.simona.test.common.model.grid.{
  BasicGrid,
  BasicGridWithSwitches,
  TransformerTestData,
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits.{DEGREE_GEOM, PU}
import edu.ie3.util.quantities.QuantityUtil
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{QuantityUtil => ScalaQuantityUtil}
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Each
import squants.electro.{Amperes, Volts}
import squants.energy.Kilowatts
import squants.space.Degrees
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units
import tech.units.indriya.unit.Units.AMPERE

import java.util.UUID
import scala.math.{cos, sin}

class GridResultsSupportSpec
    extends UnitSpec
    with GridResultsSupport
    with GridInputTestData
    with TableDrivenPropertyChecks {

  implicit val currentTolerance: squants.electro.ElectricCurrent = Amperes(1e-6)
  implicit val angleTolerance: squants.Angle = Degrees(1e-6)

  "Preparing grid results" when {
    "calculating node results" should {
      "calculate node results correctly" in {
        val nodeUuid = UUID.randomUUID
        val sweepValueStoreData = SweepValueStore.SweepValueStoreData(
          nodeUuid,
          new StateData(
            0,
            NodeType.PQ,
            Complex(0.9583756183209947, -0.04673985022513541),
            Complex(0.006466666857417822, 2.7286658176028933e-15),
          ),
        )

        val nodeResult =
          calcNodeResult(sweepValueStoreData, defaultSimulationStart)
        val expectedNodeResult = new NodeResult(
          defaultSimulationStart,
          nodeUuid,
          Quantities.getQuantity(0.9595146895129939, PU),
          Quantities
            .getQuantity(-2.79209521012981881159, DEGREE_GEOM),
        )

        nodeResult.getInputModel shouldBe expectedNodeResult.getInputModel
        nodeResult.getTime shouldBe expectedNodeResult.getTime

        QuantityUtil.isEquivalentAngle(
          nodeResult.getvAng,
          expectedNodeResult.getvAng,
          1e-12,
        ) shouldBe true
        QuantityUtil.isEquivalentAbs(
          nodeResult.getvMag,
          expectedNodeResult.getvMag,
          1e-12,
        ) shouldBe true
      }
    }

    "calculating two port connector results" should {
      "calculate switch results correctly" in new BasicGridWithSwitches {
        private val switchResult =
          calcSwitchResult(switch1, defaultSimulationStart)
        val expectedSwitchResult = new SwitchResult(
          defaultSimulationStart,
          switch1.uuid,
          switch1.isClosed,
        )

        switchResult.getTime shouldBe expectedSwitchResult.getTime
        switchResult.getInputModel shouldBe expectedSwitchResult.getInputModel
        switchResult.getClosed shouldBe expectedSwitchResult.getClosed
      }

      "calculate line results correctly" in new BasicGrid {
        line0To1.enable()
        val expectedLineResult = new LineResult(
          defaultSimulationStart,
          line0To1.uuid,
          Quantities
            .getQuantity(24.94091597114390620787, Units.AMPERE),
          Quantities
            .getQuantity(-26.42230030992568871986, DEGREE_GEOM),
          Quantities
            .getQuantity(24.9409944828817942724, Units.AMPERE),
          Quantities
            .getQuantity(153.57729374050189129708, DEGREE_GEOM),
        )

        val nodeAStateData = new StateData(
          10,
          NodeType.PQ,
          Complex(
            0.8655176782269813,
            -0.037052090894132306,
          ), // Angle = -2,4512878986765928398°
          Complex(0.319999917504236, 4.86242990316299e-15),
        )
        val nodeBStateData = new StateData(
          2,
          NodeType.PQ,
          Complex(
            0.8637364806386005,
            -0.03745498173182088,
          ), // Angle = -2,4830128149755043846°
          Complex(0.31999991750423107, -2.3469073906490223e-14),
        )

        val lineResult: LineResult = calcLineResult(
          line0To1,
          nodeAStateData,
          nodeBStateData,
          default400Kva10KvRefSystem.nominalCurrent,
          defaultSimulationStart,
        )

        lineResult.getInputModel shouldBe expectedLineResult.getInputModel

        QuantityUtil.isEquivalentAbs(
          lineResult.getiAMag,
          expectedLineResult.getiAMag,
          1e-4,
        ) shouldBe true
        QuantityUtil.isEquivalentAngle(
          lineResult.getiAAng,
          expectedLineResult.getiAAng,
          1e-3,
        ) shouldBe true

        QuantityUtil.isEquivalentAbs(
          lineResult.getiBMag,
          expectedLineResult.getiBMag,
          1e-4,
        ) shouldBe true
        QuantityUtil.isEquivalentAngle(
          lineResult.getiBAng,
          expectedLineResult.getiBAng,
          1e-3,
        ) shouldBe true

        // if line is disabled zero results are expected
        line0To1.disable()
        val disabledLineResult: LineResult = calcLineResult(
          line0To1,
          nodeAStateData,
          nodeBStateData,
          default400Kva10KvRefSystem.nominalCurrent,
          defaultSimulationStart,
        )

        disabledLineResult shouldBe new LineResult(
          defaultSimulationStart,
          line0To1.uuid,
          ScalaQuantityUtil.zeroCompQuantity(Units.AMPERE),
          ScalaQuantityUtil.zeroCompQuantity(DEGREE_GEOM),
          ScalaQuantityUtil.zeroCompQuantity(Units.AMPERE),
          ScalaQuantityUtil.zeroCompQuantity(DEGREE_GEOM),
        )
      }

      /* ATTENTION: This test uses a different test grid, as there is already a fair amount of sophisticated test data
       * available for this model */
      "calculate two winding transformer results correctly" in new TransformerTestData {
        /* Iterate over different tap positions and powers */
        forAll(tapDependentPortCurrents) {
          (
              tapSide: ConnectorPort,
              tapPos: Int,
              voltageHv: Complex,
              voltageLv: Complex,
              powerLv: Complex,
              iAMag: Double,
              iAAng: Double,
              iBMag: Double,
              iBAng: Double,
          ) =>
            /* === Prepare test data and expected result === */
            /* Get the correct transformer model */
            val transformerModel: TransformerModel = tapSide match {
              case ConnectorPort.A => transformerModelTapHv
              case ConnectorPort.B => transformerModelTapLv
              case wrongPort @ ConnectorPort.C =>
                throw InvalidTestDataException(
                  s"Received wrong test data for two winding transformer. $wrongPort is not supported as tap changer side"
                )
            }

            /* Set the correct tap changer position */
            transformerModel.updateTapPos(tapPos)

            /* Prepare node information for high voltage node */
            val hvNodeStateData = StateData(
              0,
              NodeType.SL,
              voltageHv,
              Complex.zero,
            )

            /* Prepare node information for low voltage node */
            val lvNodeStateData = StateData(
              1,
              NodeType.PQ,
              voltageLv,
              powerLv,
            )

            /* Set up grid's reference system */
            val refSys = RefSystem(
              Kilowatts(400d),
              Volts(400d),
            )

            /* Artificial time stamp */
            val time =
              TimeUtil.withDefaults.toZonedDateTime("2020-06-05T19:54:00Z")

            /* Expected result */
            val expectedResult = new Transformer2WResult(
              time,
              transformerModel.uuid,
              Quantities.getQuantity(iAMag, AMPERE),
              Quantities.getQuantity(iAAng, DEGREE_GEOM),
              Quantities.getQuantity(iBMag, AMPERE),
              Quantities.getQuantity(iBAng, DEGREE_GEOM),
              tapPos,
            )

            /* === Perform the operation to test === */
            val actual = calcTransformer2wResult(
              transformerModel,
              hvNodeStateData,
              lvNodeStateData,
              refSys.nominalCurrent,
              time,
            )

            /* === Examine the result === */
            actual.getInputModel shouldBe expectedResult.getInputModel
            QuantityUtil.isEquivalentAbs(
              actual.getiAMag(),
              expectedResult.getiAMag(),
              1e-3,
            ) shouldBe true
            QuantityUtil.isEquivalentAngle(
              actual.getiAAng(),
              expectedResult.getiAAng(),
              1e-3,
            ) shouldBe true
            QuantityUtil.isEquivalentAbs(
              actual.getiBMag(),
              expectedResult.getiBMag(),
              1e-3,
            ) shouldBe true
            if (
              QuantityUtil.isEquivalentAngle(
                actual.getiBAng(),
                expectedResult.getiBAng(),
                1e-3,
              )
            ) {
              /* Angles are considerably equal */
              succeed
            } else {
              /* Angles are different. Especially for small current magnitudes, the angle does not play a too vital role.
               * Therefore, we will check for the cartesian participants of the complex magnitude. This "workaround"
               * especially is applicable here. The expected results have been generated using pandapower
               * (https://www.pandapower.org/), which only reveals current angles by means of active and reactive port
               * powers. Thereby rounding errors could propagate up to this point. */
              /* Testing the real part of the current */
              QuantityUtil.isEquivalentAbs(
                actual
                  .getiBMag()
                  .multiply(
                    cos(
                      actual
                        .getiBAng()
                        .to(DEGREE_GEOM)
                        .getValue
                        .doubleValue()
                        .toRadians
                    )
                  )
                  .to(AMPERE),
                expectedResult
                  .getiBMag()
                  .multiply(
                    cos(
                      expectedResult
                        .getiBAng()
                        .to(DEGREE_GEOM)
                        .getValue
                        .doubleValue()
                        .toRadians
                    )
                  )
                  .to(AMPERE),
                1e-4,
              ) shouldBe true
              /* Testing the imaginary part of the current */
              QuantityUtil.isEquivalentAbs(
                actual
                  .getiBMag()
                  .multiply(
                    sin(
                      actual
                        .getiBAng()
                        .to(DEGREE_GEOM)
                        .getValue
                        .doubleValue()
                        .toRadians
                    )
                  )
                  .to(AMPERE),
                expectedResult
                  .getiBMag()
                  .multiply(
                    sin(
                      expectedResult
                        .getiBAng()
                        .to(DEGREE_GEOM)
                        .getValue
                        .doubleValue()
                        .toRadians
                    )
                  )
                  .to(AMPERE),
                1e-4,
              ) shouldBe true
            }
            actual.getTapPos shouldBe expectedResult.getTapPos
            actual.getTime shouldBe expectedResult.getTime
        }
      }

      /* ATTENTION: This test uses a different test grid, as there is already a fair amount of sophisticated test data
       * available for this model */
      "return correct dummy result on disabled two winding transformer" in new TransformerTestData {
        val transformerModel: TransformerModel = transformerModelTapHv
        transformerModel.disable()

        val nodeStateDataHv: StateData = StateData(
          0,
          NodeType.SL,
          Complex.one,
          Complex.zero,
        )
        val nodeStateDataLv: StateData = StateData(
          1,
          NodeType.PQ,
          Complex.one,
          Complex.zero,
        )

        val expectedResult: Transformer2WResult = new Transformer2WResult(
          TimeUtil.withDefaults.toZonedDateTime("2020-06-08T09:03:00Z"),
          transformerModel.uuid,
          ScalaQuantityUtil.zeroCompQuantity(AMPERE),
          ScalaQuantityUtil.zeroCompQuantity(DEGREE_GEOM),
          ScalaQuantityUtil.zeroCompQuantity(AMPERE),
          ScalaQuantityUtil.zeroCompQuantity(DEGREE_GEOM),
          transformerModel.currentTapPos,
        )

        calcTransformer2wResult(
          transformerModel,
          nodeStateDataHv,
          nodeStateDataLv,
          RefSystem(
            Kilowatts(400d),
            Volts(400d),
          ).nominalCurrent,
          TimeUtil.withDefaults.toZonedDateTime("2020-06-08T09:03:00Z"),
        ) shouldBe expectedResult
      }
    }

    "calculating three winding transformer results" should {
      val hvNode = UUID.randomUUID()
      val mvNode = UUID.randomUUID()
      val lvNode = UUID.randomUUID()
      val nodeInternal = UUID.randomUUID()
      val transformerA = Transformer3wModel(
        UUID.randomUUID(),
        "test_transformer",
        OperationInterval(0L, 3600L),
        hvNode,
        mvNode,
        lvNode,
        nodeInternal,
        BigDecimal("1"),
        TransformerTappingModel(
          Quantities.getQuantity(1.5, StandardUnits.DV_TAP),
          5,
          10,
          -10,
          0,
          autoTap = true,
        ),
        1,
        PowerFlowCaseA,
        Each(0.1d),
        Each(0.2d),
        Each(0.3d),
        Each(0.4d),
      )
      transformerA.initTapping()
      val transformerB = transformerA.copy(
        powerFlowCase = PowerFlowCaseB,
        g = Each(0d),
        b = Each(0d),
      )
      val transformerC = transformerA.copy(
        powerFlowCase = PowerFlowCaseC,
        g = Each(0d),
        b = Each(0d),
      )
      val iNominal = Amperes(100d)

      val timeStamp =
        TimeUtil.withDefaults.toZonedDateTime("2021-06-10T14:45:00Z")
      "assemble correct result for transformer at node A" in {
        val nodeStateData =
          StateData(0, NodeType.SL, Complex(1.0, 0.0), Complex.zero)
        val internalNodeStateData =
          StateData(0, NodeType.PQ, Complex(0.97, -0.01), Complex.zero)
        calcTransformer3wResult(
          transformerA,
          nodeStateData,
          internalNodeStateData,
          iNominal,
          timeStamp,
        ) match {
          case PartialTransformer3wResult.PortA(
                time,
                input,
                currentMagnitude,
                currentAngle,
                tapPos,
              ) =>
            time shouldBe timeStamp
            input shouldBe transformerA.uuid
            tapPos shouldBe transformerA.currentTapPos
            currentMagnitude should approximate(Amperes(13.15547500d))
            currentAngle should approximate(Degrees(-45.0000000d))
          case wrong => fail(s"Got wrong result: '$wrong'")
        }
      }

      "assemble correct result for transformer at node B" in {
        val nodeStateData =
          StateData(0, NodeType.PQ, Complex(0.97, -0.01), Complex.zero)
        val internalNodeStateData =
          StateData(0, NodeType.SL, Complex(1.0, 0.0), Complex.zero)
        calcTransformer3wResult(
          transformerB,
          nodeStateData,
          internalNodeStateData,
          iNominal,
          timeStamp,
        ) match {
          case PartialTransformer3wResult.PortB(
                time,
                input,
                currentMagnitude,
                currentAngle,
              ) =>
            time shouldBe timeStamp
            input shouldBe transformerB.uuid
            currentMagnitude should approximate(Amperes(14.14213562d))
            currentAngle should approximate(Degrees(135.000000d))
          case wrong => fail(s"Got wrong result: '$wrong'")
        }
      }

      "assemble correct result for transformer at node C" in {
        val nodeStateData =
          StateData(0, NodeType.PQ, Complex(0.97, -0.01), Complex.zero)
        val internalNodeStateData =
          StateData(0, NodeType.SL, Complex(1.0, 0.0), Complex.zero)
        calcTransformer3wResult(
          transformerC,
          nodeStateData,
          internalNodeStateData,
          iNominal,
          timeStamp,
        ) match {
          case PartialTransformer3wResult.PortC(
                time,
                input,
                currentMagnitude,
                currentAngle,
              ) =>
            time shouldBe timeStamp
            input shouldBe transformerC.uuid
            currentMagnitude should approximate(Amperes(14.14213562d))
            currentAngle should approximate(Degrees(135.0000000d))
          case wrong => fail(s"Got wrong result: '$wrong'")
        }
      }

      "calculate three winding model correctly for a real model" in new DefaultTestData {
        calcTransformer3wResult(
          Transformer3wModel(
            transformer3wInput,
            default400Kva10KvRefSystem,
            2,
            defaultSimulationStart,
            defaultSimulationEnd,
          ),
          new StateData(0, NodeType.PQ, Complex(1, 0), Complex(1, 0)),
          new StateData(1, NodeType.PQ, Complex(0.99, 0), Complex(0.98, 0)),
          default400Kva10KvRefSystem.nominalCurrent,
          timeStamp,
        ) match {
          case PartialTransformer3wResult.PortB(
                time,
                input,
                currentMagnitude,
                currentAngle,
              ) =>
            time shouldBe timeStamp
            input shouldBe transformer3wInput.getUuid
            currentMagnitude should approximate(Amperes(11.4542161d))
            currentAngle should approximate(Degrees(-89.4475391d))
          case wrong => fail(s"Got wrong result: '$wrong'")
        }
      }
    }
  }
}
