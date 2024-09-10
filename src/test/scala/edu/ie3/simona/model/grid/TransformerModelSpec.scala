/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import breeze.numerics.abs
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.datamodel.models.input.connector.{
  ConnectorPort,
  Transformer2WInput,
}
import edu.ie3.powerflow.NewtonRaphsonPF
import edu.ie3.powerflow.model.NodeData.{PresetData, StateData}
import edu.ie3.powerflow.model.StartData.WithForcedStartVoltages
import edu.ie3.powerflow.model.enums.NodeType
import edu.ie3.powerflow.model.{NodeData, PowerFlowResult}
import edu.ie3.simona.test.common.model.grid.{
  TapTestData,
  TransformerTestData,
  TransformerTestGrid,
}
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.util.quantities.PowerSystemUnits._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor4}
import squants.Each
import squants.electro.Amperes
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID

class TransformerModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with ConfigTestData {
  val quantityTolerance: Double = 1e-5
  val testingTolerancePf = 1e-9
  implicit val electricCurrentTolerance: squants.electro.ElectricCurrent =
    Amperes(1e-9)
  implicit val dimensionlessTolerance: squants.Dimensionless = Each(1e-9)

  "A valid TransformerInput " should {
    "be validated without an exception" in new TransformerTestData {
      val unmodifiedTransformerInputModel: Transformer2WInput =
        transformerInputTapHv

      noException shouldBe thrownBy {
        TransformerModel.validateInputModel(
          unmodifiedTransformerInputModel,
          mainRefSystem,
        )
      }
    }

    "result in a valid TransformerModel" in new TransformerTestData {
      val inputModel: Transformer2WInput = transformerInputTapHv
      val validTransformerTappingModel: TransformerTappingModel =
        TransformerTappingModel(
          inputModel.getType.getdV(),
          inputModel.getTapPos,
          inputModel.getType.getTapMax,
          inputModel.getType.getTapMin,
          inputModel.getType.getTapNeutr,
          inputModel.isAutoTap, {
            if (inputModel.getType.isTapSide)
              ConnectorPort.B
            else
              ConnectorPort.A
          },
        )

      val dut: TransformerModel =
        TransformerModel(
          inputModel,
          mainRefSystem,
          defaultSimulationStart,
          defaultSimulationEnd,
        )

      inside(dut) {
        case TransformerModel(
              uuid: UUID,
              id,
              operationInterval,
              hvNodeUuid,
              lvNodeUuid,
              transformerTappingModel,
              amount,
              voltRatioNominal,
              iNomHv,
              iNomLv,
              sRated,
              r,
              x,
              g,
              b,
            ) =>
          uuid should be(inputModel.getUuid)
          id should be(inputModel.getId)
          operationInterval should be(defaultOperationInterval)
          hvNodeUuid should be(inputModel.getNodeA.getUuid)
          lvNodeUuid should be(inputModel.getNodeB.getUuid)

          transformerTappingModel should be(validTransformerTappingModel)
          /* Have an insight into the tapping model */
          inside(transformerTappingModel) {
            case TransformerTappingModel(
                  _,
                  _currentTapPos,
                  _,
                  _,
                  _,
                  _,
                  tapSide,
                ) =>
              _currentTapPos shouldBe 0
              tapSide shouldBe ConnectorPort.A
          }

          amount shouldBe inputModel.getParallelDevices
          voltRatioNominal shouldBe BigDecimal("25")
          iNomHv should approximate(Amperes(36.373066958946424d))
          iNomLv should approximate(Amperes(909.3266739736606d))
          sRated shouldBe Kilowatts(630)
          r should approximate(Each(7.357e-3))
          x should approximate(Each(24.30792e-3))
          g should approximate(Each(0.0))
          b should approximate(Each(-3.75e-3))
      }

      /* The following tests are with regard to the tap position = 0 */
      val (gii, bii) =
        TransformerModel.y0(dut, ConnectorPort.A) match {
          case Complex(g, b) => (g, b)
        }
      (abs(gii - 0.0) < quantityTolerance) shouldBe true
      (abs(bii - (-1.875e-3)) < quantityTolerance) shouldBe true

      val (gjj, bjj) =
        TransformerModel.y0(dut, ConnectorPort.B) match {
          case Complex(g, b) => (g, b)
        }
      (abs(gjj - 0.0) < quantityTolerance) shouldBe true
      (abs(bjj - (-1.875e-3)) < quantityTolerance) shouldBe true

      val (gij, bij) =
        TransformerModel.yij(dut) match {
          case Complex(g, b) => (g, b)
        }
      (abs(gij - 11.40619406) < quantityTolerance) shouldBe true
      (abs(bij - (-37.68667292)) < quantityTolerance) shouldBe true
    }

    "result in an enabled TransformerModel if the TransformerInputModel is enabled" in new TransformerTestData {
      val transformer2w: TransformerModel =
        TransformerModel(
          transformerInputTapHv,
          mainRefSystem,
          defaultSimulationStart,
          defaultSimulationEnd,
        )

      transformer2w.isInOperation shouldBe true
    }

    "result in an disabled TransformerModel if the TransformerInputModel is disabled" in new TransformerTestData {
      val earlySimulationStart: ZonedDateTime =
        defaultSimulationStart.minus(1, ChronoUnit.HOURS)
      val transformer2w: TransformerModel =
        TransformerModel(
          transformerInputTapHv,
          mainRefSystem,
          earlySimulationStart,
          defaultSimulationEnd,
        )

      transformer2w.isInOperation shouldBe false
    }

    "initialize its tapping capabilities automatically" in new TransformerTestData {
      val tapRatio: PrivateMethod[Double] =
        PrivateMethod[Double](Symbol("tapRatio"))

      val dut: TransformerModel =
        TransformerModel(
          transformerInputTapHv,
          mainRefSystem,
          defaultSimulationStart,
          defaultSimulationEnd,
        )

      dut invokePrivate tapRatio() shouldBe 1.0
    }
  }

  "A valid transformer model" should {
    "be able to be enabled and disabled on request" in new TransformerTestGrid {
      transformerModelTapHv.disable().isSuccess shouldBe true
      transformerModelTapHv.isInOperation shouldBe false

      transformerModelTapHv.disable().isFailure shouldBe true
      transformerModelTapHv.isInOperation shouldBe false

      transformerModelTapHv.enable().isSuccess shouldBe true
      transformerModelTapHv.isInOperation shouldBe true

      transformerModelTapHv.enable().isFailure shouldBe true
      transformerModelTapHv.isInOperation shouldBe true
    }

    "change its tap position on request and return a valid tap ratio afterwards" in new TransformerTestGrid {

      val tapRatio: PrivateMethod[Double] =
        PrivateMethod[Double](Symbol("tapRatio"))

      transformerModelTapHv.currentTapPos shouldBe 0
      transformerModelTapHv invokePrivate tapRatio() shouldBe 1.0

      transformerModelTapHv.incrTapPos()

      transformerModelTapHv.currentTapPos shouldBe 1
      transformerModelTapHv invokePrivate tapRatio() shouldBe 1.025

      transformerModelTapHv.incrTapPos(4)

      transformerModelTapHv.currentTapPos shouldBe 5
      transformerModelTapHv invokePrivate tapRatio() shouldBe 1.125

      transformerModelTapHv.updateTapPos(6)

      transformerModelTapHv.currentTapPos shouldBe 6
      transformerModelTapHv invokePrivate tapRatio() shouldBe 1.15

      transformerModelTapHv.decrTapPos()

      transformerModelTapHv.currentTapPos shouldBe 5
      transformerModelTapHv invokePrivate tapRatio() shouldBe 1.125

      transformerModelTapHv.decrTapPos(3)

      transformerModelTapHv.currentTapPos shouldBe 2
      transformerModelTapHv invokePrivate tapRatio() shouldBe 1.05
    }

    "should compute valid delta tap positions" in new TransformerTestGrid {
      val cases: TableFor4[Int, Double, Double, Int] =
        Table(
          ("currentTapPos", "vChange", "deadBand", "expected"),
          (0, 0.025d, 0.75d, 1), /* Simple step up */
          (0, 0.04d, 0.75d, 1), /* Remainder beneath dead band */
          (0, 0.04d, 0.3d, 2), /* Remainder above dead band */
          (8, 0.08d, 0.75d, 2),
          /* Limit to max tap (should be 3 limited to 2) */
          (0, -0.025d, 0.75d, -1), /* Simple step down */
          (0, -0.04d, 0.75d, -1), /* Remainder beneath dead band */
          (0, -0.04d, 0.3d, -2), /* Remainder above dead band */
          (
            -8,
            -0.08d,
            0.75d,
            -2,
          ), /* Limit to min tap (should be -3 limited to -2) */
        )

      forAll(cases) {
        (
            currentTapPos: Int,
            vChangeVal: Double,
            deadBandVal: Double,
            expected: Int,
        ) =>
          {
            val vChange = Quantities.getQuantity(vChangeVal, PU)
            val deadBand = Quantities.getQuantity(deadBandVal, PU)

            transformerModelTapHv.updateTapPos(currentTapPos)
            transformerModelTapHv.computeDeltaTap(
              vChange,
              deadBand = deadBand,
            ) shouldBe expected
          }
      }
    }

    "should calculate the correct tap dependent equivalent circuit diagram parameters" in new TapTestData {
      forAll(tapDependentEquivalentCircuitParameters) {
        (
            tapSide: ConnectorPort,
            tapPos: Int,
            yijExpected: Complex,
            yiiExpected: Complex,
            yjjExpected: Complex,
        ) =>
          {
            val transformer = tapSide match {
              case ConnectorPort.A => transformerModelTapHv
              case ConnectorPort.B => transformerModelTapLv
              case _ =>
                throw new InvalidGridException(
                  s"Cannot find a transformer for tap side $tapSide in the basic grid"
                )
            }

            transformer.updateTapPos(tapPos)
            val (gijActual, bijActual) =
              TransformerModel.yij(transformer) match {
                case Complex(g, b) => (g, b)
              }
            val (giiActual, biiActual) =
              TransformerModel.y0(transformer, ConnectorPort.A) match {
                case Complex(g, b) => (g, b)
              }
            val (gjjActual, bjjActual) =
              TransformerModel.y0(transformer, ConnectorPort.B) match {
                case Complex(g, b) => (g, b)
              }

            (abs(
              gijActual - yijExpected.real
            ) < quantityTolerance) shouldBe true
            (abs(
              bijActual - yijExpected.imag
            ) < quantityTolerance) shouldBe true
            (abs(
              giiActual - yiiExpected.real
            ) < quantityTolerance) shouldBe true
            (abs(
              biiActual - yiiExpected.imag
            ) < quantityTolerance) shouldBe true
            (abs(
              gjjActual - yjjExpected.real
            ) < quantityTolerance) shouldBe true
            (abs(
              bjjActual - yjjExpected.imag
            ) < quantityTolerance) shouldBe true
          }
      }
    }
  }

  "A transformer in a simple grid" should {
    "lead to the correct voltages at specific loadings" in new TransformerTestData {
      /* Fix the slack node to 1 + j0 pu as slack voltage */
      val startData: Option[WithForcedStartVoltages] = Option.apply(
        WithForcedStartVoltages(
          Array(StateData(0, NodeType.SL, Complex.one, Complex.zero))
        )
      )

      forAll(tapDependentNodalVoltage) {
        (
            tapSide: ConnectorPort,
            tapPos: Int,
            p: BigDecimal,
            e: Double,
            f: Double,
        ) =>
          {
            logger.debug(
              if (tapSide == ConnectorPort.A)
                s"Test grid with transformer tap changer hat HV side at tapPos $tapPos and active power of $p p.u."
              else
                s"Test grid with transformer tap changer hat LV side at tapPos $tapPos and active power of $p p.u."
            )

            val grid = tapSide match {
              case ConnectorPort.A => gridTapHv
              case ConnectorPort.B => gridTapLv
              case unknown =>
                throw new InvalidGridException(
                  s"Cannot test a transformer with tap changer at $unknown"
                )
            }
            val gridModel = GridModel(
              grid,
              refSystem,
              voltageLimits,
              defaultSimulationStart,
              defaultSimulationEnd,
              simonaConfig,
            )

            gridModel.gridComponents.transformers
              .toVector(0)
              .updateTapPos(tapPos)
            val admittanceMatrix =
              GridModel.composeAdmittanceMatrix(
                nodeUuidToIndexMap,
                gridModel.gridComponents,
              )

            val operationPoint =
              Array(
                PresetData(0, NodeType.SL, Complex.zero),
                PresetData(1, NodeType.PQ, Complex(p.doubleValue, 0d)),
              )

            val powerFlow = new NewtonRaphsonPF(
              epsilon,
              maxIterations,
              admittanceMatrix,
              Option.apply(Vector(0, 1)),
            )
            val result = powerFlow.calculate(operationPoint, startData)

            result match {
              case success: PowerFlowResult.SuccessFullPowerFlowResult =>
                val v = NodeData
                  .getByIndex(success.nodeData, 1)
                  .voltage
                abs(v.real - e) < testingTolerancePf shouldBe true
                abs(v.imag - f) < testingTolerancePf shouldBe true
              case _: PowerFlowResult.FailedPowerFlowResult =>
                fail(
                  if (tapSide == ConnectorPort.A)
                    s"Unable to calculate the power flow on transformer tap position $tapPos (tap changer on HV side) and active power of $p p.u."
                  else
                    s"Unable to calculate the power flow on transformer tap position $tapPos (tap changer on LV side) and active power of $p p.u."
                )
            }
          }
      }
    }
  }
}
