/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import breeze.numerics.abs
import edu.ie3.simona.exceptions.InvalidActionRequestException
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseA,
  PowerFlowCaseB,
  PowerFlowCaseC
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.Transformer3wTestData
import edu.ie3.util.quantities.PowerSystemUnits._
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor4}
import tech.units.indriya.quantity.Quantities

import scala.math.BigDecimal.RoundingMode

class Transformer3wModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with Transformer3wTestData {
  val testingTolerance = 1e-5

  "A three winding transformer input model" should {
    "be validated without an exception from a valid input model" in {
      Transformer3wModel.validateInputModel(transformer3wInput)
    }

    "result in a valid three winding transformer model - hv side" in new Transformer3wTestData {
      val expectedTappingModel: TransformerTappingModel =
        TransformerTappingModel(
          transformer3wInput.getType.getdV(),
          transformer3wInput.getTapPos,
          transformer3wInput.getType.getTapMax,
          transformer3wInput.getType.getTapMin,
          transformer3wInput.getType.getTapNeutr,
          transformer3wInput.isAutoTap
        )

      val transformerModel: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemEhv,
          1,
          defaultSimulationStart,
          defaultSimulationEnd
        )

      inside(transformerModel) {
        case Transformer3wModel(
              uuid,
              id,
              operationInterval,
              hvNodeUuid,
              mvNodeUuid,
              lvNodeUuid,
              nodeInternalUuid,
              voltRatioNominal,
              transformerTappingModel,
              amount,
              powerFlowCase,
              r,
              x,
              g,
              b
            ) =>
          uuid shouldBe transformer3wInput.getUuid
          id shouldBe transformer3wInput.getId
          operationInterval shouldBe defaultOperationInterval
          hvNodeUuid shouldBe transformer3wInput.getNodeA.getUuid
          mvNodeUuid shouldBe transformer3wInput.getNodeB.getUuid
          lvNodeUuid shouldBe transformer3wInput.getNodeC.getUuid
          nodeInternalUuid shouldBe transformer3wInput.getNodeInternal.getUuid
          voltRatioNominal shouldBe BigDecimal
            .apply("1.0")
            .setScale(5, RoundingMode.HALF_UP)
          transformerTappingModel shouldBe expectedTappingModel
          amount shouldBe transformer3wInput.getParallelDevices
          powerFlowCase shouldBe PowerFlowCaseA
          abs(r.to(PU).getValue.doubleValue() - 2.07756e-3) < 1e-8 shouldBe true
          abs(x.to(PU).getValue.doubleValue() - 6.92521e-3) < 1e-8 shouldBe true
          abs(
            g.to(PU).getValue.doubleValue() - 5.77600e-6
          ) < 1e-11 shouldBe true
          abs(
            b.to(PU).getValue.doubleValue() + 144.3999e-9
          ) < 1e-11 shouldBe true
      }

      val yii: Complex = Transformer3wModel.y0(
        transformerModel,
        Transformer3wModel.Transformer3wPort.A
      )
      yii shouldBe Complex.zero
      val yjj: Complex =
        Transformer3wModel.y0(
          transformerModel,
          Transformer3wModel.Transformer3wPort.INTERNAL
        )
      abs(yjj.real - 5.77600e-6) < 1e-11 shouldBe true
      abs(yjj.imag + 144.3999e-9) < 1e-11 shouldBe true
      val yij: Complex = Transformer3wModel.yij(transformerModel)
      abs(yij.real - 39.743119) < testingTolerance shouldBe true
      abs(yij.imag + 132.477064) < testingTolerance shouldBe true
    }

    "result in a valid three winding transformer model - mv side" in new Transformer3wTestData {
      val expectedTappingModel: TransformerTappingModel =
        TransformerTappingModel(
          transformer3wInput.getType.getdV(),
          transformer3wInput.getTapPos,
          transformer3wInput.getType.getTapMax,
          transformer3wInput.getType.getTapMin,
          transformer3wInput.getType.getTapNeutr,
          transformer3wInput.isAutoTap
        )

      val transformerModel: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd
        )

      inside(transformerModel) {
        case Transformer3wModel(
              uuid,
              id,
              operationInterval,
              hvNodeUuid,
              mvNodeUuid,
              lvNodeUuid,
              nodeInternalUuid,
              voltRatioNominal,
              transformerTappingModel,
              amount,
              powerFlowCase,
              r,
              x,
              g,
              b
            ) =>
          uuid shouldBe transformer3wInput.getUuid
          id shouldBe transformer3wInput.getId
          operationInterval shouldBe defaultOperationInterval
          hvNodeUuid shouldBe transformer3wInput.getNodeA.getUuid
          mvNodeUuid shouldBe transformer3wInput.getNodeB.getUuid
          lvNodeUuid shouldBe transformer3wInput.getNodeC.getUuid
          nodeInternalUuid shouldBe transformer3wInput.getNodeInternal.getUuid
          voltRatioNominal shouldBe BigDecimal
            .apply("3.45455")
            .setScale(5, RoundingMode.HALF_UP)
          transformerTappingModel shouldBe expectedTappingModel
          amount shouldBe transformer3wInput.getParallelDevices
          powerFlowCase shouldBe PowerFlowCaseB
          abs(r.to(PU).getValue.doubleValue() - 1.47917e-3) < 1e-8 shouldBe true
          abs(x.to(PU).getValue.doubleValue() - 4.73410e-3) < 1e-8 shouldBe true
          g shouldBe Quantities.getQuantity(0d, PU)
          b shouldBe Quantities.getQuantity(0d, PU)
      }

      val yii: Complex = Transformer3wModel.y0(
        transformerModel,
        Transformer3wModel.Transformer3wPort.A
      )
      yii shouldBe Complex.zero
      val yjj: Complex =
        Transformer3wModel.y0(
          transformerModel,
          Transformer3wModel.Transformer3wPort.INTERNAL
        )
      yjj shouldBe Complex.zero
      val yij: Complex = Transformer3wModel.yij(transformerModel)
      abs(yij.real - 60.129748) < testingTolerance shouldBe true
      abs(yij.imag + 192.445633) < testingTolerance shouldBe true
    }

    "result in a valid three winding transformer model - lv side" in new Transformer3wTestData {
      val expectedTappingModel: TransformerTappingModel =
        TransformerTappingModel(
          transformer3wInput.getType.getdV(),
          transformer3wInput.getTapPos,
          transformer3wInput.getType.getTapMax,
          transformer3wInput.getType.getTapMin,
          transformer3wInput.getType.getTapNeutr,
          transformer3wInput.isAutoTap
        )

      val transformerModel: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd
        )

      inside(transformerModel) {
        case Transformer3wModel(
              uuid,
              id,
              operationInterval,
              hvNodeUuid,
              mvNodeUuid,
              lvNodeUuid,
              nodeInternalUuid,
              voltRatioNominal,
              transformerTappingModel,
              amount,
              powerFlowCase,
              r,
              x,
              g,
              b
            ) =>
          uuid shouldBe transformer3wInput.getUuid
          id shouldBe transformer3wInput.getId
          operationInterval shouldBe defaultOperationInterval
          hvNodeUuid shouldBe transformer3wInput.getNodeA.getUuid
          mvNodeUuid shouldBe transformer3wInput.getNodeB.getUuid
          lvNodeUuid shouldBe transformer3wInput.getNodeC.getUuid
          nodeInternalUuid shouldBe transformer3wInput.getNodeInternal.getUuid
          voltRatioNominal shouldBe BigDecimal
            .apply("19.0")
            .setScale(5, RoundingMode.HALF_UP)
          transformerTappingModel shouldBe expectedTappingModel
          amount shouldBe transformer3wInput.getParallelDevices
          powerFlowCase shouldBe PowerFlowCaseC
          abs(r.to(PU).getValue.doubleValue() - 288.8e-6) < 1e-8 shouldBe true
          abs(x.to(PU).getValue.doubleValue() - 1.083e-3) < 1e-8 shouldBe true
          g shouldBe Quantities.getQuantity(0d, PU)
          b shouldBe Quantities.getQuantity(0d, PU)
      }

      val yii: Complex = Transformer3wModel.y0(
        transformerModel,
        Transformer3wModel.Transformer3wPort.A
      )
      yii shouldBe Complex.zero
      val yjj: Complex =
        Transformer3wModel.y0(
          transformerModel,
          Transformer3wModel.Transformer3wPort.INTERNAL
        )
      yjj shouldBe Complex.zero
      val yij: Complex = Transformer3wModel.yij(transformerModel)
      abs(yij.real - 229.882415) < testingTolerance shouldBe true
      abs(yij.imag + 862.059057) < testingTolerance shouldBe true
    }

    "result in an enabled three winding transformer model, if the input model is enabled at simulation start" in new Transformer3wTestData {
      val transformerModelEhvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemEhv,
          1,
          defaultSimulationStart,
          defaultSimulationEnd
        )
      val transformerModelHvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd
        )
      val transformerModelLvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd
        )

      transformerModelEhvTemp.isInOperation shouldBe true
      transformerModelHvTemp.isInOperation shouldBe true
      transformerModelLvTemp.isInOperation shouldBe true
    }

    "result in an disabled three winding transformer model, if the input model is disabled at simulation start" in new Transformer3wTestData {
      val transformerModelEhvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputPostponed,
          mainRefSystemEhv,
          1,
          defaultSimulationStart,
          defaultSimulationEnd
        )
      val transformerModelHvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputPostponed,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd
        )
      val transformerModelLvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputPostponed,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd
        )

      transformerModelEhvTemp.isInOperation shouldBe false
      transformerModelHvTemp.isInOperation shouldBe false
      transformerModelLvTemp.isInOperation shouldBe false
    }

    "initialize its tapping capabilities automatically" in new Transformer3wTestData {
      val tapRatio: PrivateMethod[Double] =
        PrivateMethod[Double](Symbol("tapRatio"))

      val transformerModelEhvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputTapped,
          mainRefSystemEhv,
          1,
          defaultSimulationStart,
          defaultSimulationEnd
        )
      val transformerModelHvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputTapped,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd
        )
      val transformerModelLvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputTapped,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd
        )

      transformerModelEhvTemp invokePrivate tapRatio() shouldBe 1.15
      transformerModelHvTemp invokePrivate tapRatio() shouldBe 1.0
      transformerModelLvTemp invokePrivate tapRatio() shouldBe 1.0
    }
  }

  "A valid transformer model" should {
    "be able to be enabled and disabled on request" in new Transformer3wTestData {
      val transformerModel: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemEhv,
          1,
          defaultSimulationStart,
          defaultSimulationEnd
        )

      transformerModel.disable().isSuccess shouldBe true
      transformerModel.isInOperation shouldBe false

      transformerModel.disable().isFailure shouldBe true
      transformerModel.isInOperation shouldBe false

      transformerModel.enable().isSuccess shouldBe true
      transformerModel.isInOperation shouldBe true

      transformerModel.enable().isFailure shouldBe true
      transformerModel.isInOperation shouldBe true
    }

    "change its tap position on request and return a valid tap ratio afterwards" in new Transformer3wTestData {
      val transformerModel: Transformer3wModel = transformerModelEhv
      val tapRatio: PrivateMethod[Double] =
        PrivateMethod[Double](Symbol("tapRatio"))

      transformerModel.currentTapPos shouldBe 0
      transformerModel invokePrivate tapRatio() shouldBe 1.0

      transformerModel.incrTapPos()

      transformerModel.currentTapPos shouldBe 1
      transformerModel invokePrivate tapRatio() shouldBe 1.015

      transformerModel.incrTapPos(4)

      transformerModel.currentTapPos shouldBe 5
      transformerModel invokePrivate tapRatio() shouldBe 1.075

      transformerModel.updateTapPos(6)

      transformerModel.currentTapPos shouldBe 6
      transformerModel invokePrivate tapRatio() shouldBe 1.09

      transformerModel.decrTapPos()

      transformerModel.currentTapPos shouldBe 5
      transformerModel invokePrivate tapRatio() shouldBe 1.075

      transformerModel.decrTapPos(3)

      transformerModel.currentTapPos shouldBe 2
      transformerModel invokePrivate tapRatio() shouldBe 1.03
    }

    "dislike altering the tap position in power flow case B" in new Transformer3wTestData {
      val transformerModel: Transformer3wModel = transformerModelHv
      val tapRatio: PrivateMethod[Double] =
        PrivateMethod[Double](Symbol("tapRatio"))

      transformerModel.currentTapPos shouldBe 0
      transformerModel invokePrivate tapRatio() shouldBe 1.0

      val thrownOnIncrease: InvalidActionRequestException =
        intercept[InvalidActionRequestException] {
          transformerModel.incrTapPos()
        }
      thrownOnIncrease.getMessage shouldBe s"Increasing tap position for transformer3w ${transformerModel.uuid} is not allowed in power flow case B and C."
      transformerModel.currentTapPos shouldBe 0
      transformerModel invokePrivate tapRatio() shouldBe 1.0

      val thrownOnDecrease: InvalidActionRequestException =
        intercept[InvalidActionRequestException] {
          transformerModel.decrTapPos()
        }
      thrownOnDecrease.getMessage shouldBe s"Decreasing tap position for transformer3w ${transformerModel.uuid} is not allowed in power flow case B and C."
      transformerModel.currentTapPos shouldBe 0
      transformerModel invokePrivate tapRatio() shouldBe 1.0

      val thrownOnUpdate: InvalidActionRequestException =
        intercept[InvalidActionRequestException] {
          transformerModel.updateTapPos(6)
        }
      thrownOnUpdate.getMessage shouldBe s"Updating tap position for transformer3w ${transformerModel.uuid} is not allowed in power flow case B and C."
      transformerModel.currentTapPos shouldBe 0
      transformerModel invokePrivate tapRatio() shouldBe 1.0
    }

    "should compute valid delta tap positions" in new Transformer3wTestData {
      val cases: TableFor4[Int, Double, Double, Int] =
        Table(
          ("currentTapPos", "vChange", "deadBand", "expected"),
          (0, 0.015d, 0.75d, 1), /* Simple step up */
          (0, 0.025d, 0.75d, 1), /* Remainder beneath dead band */
          (0, 0.025d, 0.3d, 2), /* Remainder above dead band */
          (8, 0.05d, 0.75d, 2),
          /* Limit to max tap (should be 3 limited to 2) */
          (0, -0.015d, 0.75d, -1), /* Simple step down */
          (0, -0.025d, 0.75d, -1), /* Remainder beneath dead band */
          (0, -0.025d, 0.3d, -2), /* Remainder above dead band */
          (
            -8,
            -0.05d,
            0.75d,
            -2
          ) /* Limit to min tap (should be -3 limited to -2) */
        )

      val transformerModel: Transformer3wModel = transformerModelEhv

      forAll(cases) {
        (
            currentTapPos: Int,
            vChangeVal: Double,
            deadBandVal: Double,
            expected: Int
        ) =>
          {
            val vChange = Quantities.getQuantity(vChangeVal, PU)
            val deadBand = Quantities.getQuantity(deadBandVal, PU)

            transformerModel.updateTapPos(currentTapPos)
            val actual = transformerModel.computeDeltaTap(vChange, deadBand)
            actual should be(expected)
          }
      }
    }

    "should calculate the correct tap dependent equivalent circuit diagram parameters - EHV side" in new Transformer3wTestData {
      val transformer: Transformer3wModel = transformerModelEhv
      forAll(tapDependentEquivalentCircuitParametersEhv) {
        (
            tapPos: Int,
            yijExpected: Complex,
            yiiExpected: Complex,
            yjjExpected: Complex
        ) =>
          {
            transformer.updateTapPos(tapPos)
            val yijActual = Transformer3wModel.yij(transformer)
            val yiiActual = Transformer3wModel.y0(
              transformer,
              Transformer3wModel.Transformer3wPort.A
            )
            val yjjActual =
              Transformer3wModel.y0(
                transformer,
                Transformer3wModel.Transformer3wPort.INTERNAL
              )

            /* Remark: This is not really precise. At the moment, double-based calculations do
             * hinder us from being more precise. Maybe it is advisory to switch over to BigDecimal */
            (abs(yijActual.real - yijExpected.real) < 1e-4) shouldBe true
            (abs(yijActual.imag - yijExpected.imag) < 1e-4) shouldBe true
            (abs(yiiActual.real - yiiExpected.real) < 1e-4) shouldBe true
            (abs(yiiActual.imag - yiiExpected.imag) < 1e-4) shouldBe true
            (abs(yjjActual.real - yjjExpected.real) < 1e-4) shouldBe true
            (abs(yjjActual.imag - yjjExpected.imag) < 1e-4) shouldBe true
          }
      }
    }

    "should calculate the correct tap dependent voltage ratio - EHV side" in new Transformer3wTestData {
      val transformer: Transformer3wModel = transformerModelEhv
      forAll(tapDependentVoltRatioEhv) { (tapPos, expectedVoltRatioStr) =>
        val expectedVoltRatio = BigDecimal
          .apply(expectedVoltRatioStr)
          .setScale(5, RoundingMode.HALF_UP)
        transformer.updateTapPos(tapPos)
        val actualVoltRatio = Transformer3wModel.voltRatio(transformer)

        actualVoltRatio shouldBe expectedVoltRatio
      }
    }

    "should calculate the correct tap dependent voltage ratio - HV side" in new Transformer3wTestData {
      val transformer: Transformer3wModel = transformerModelHv
      val expectedVoltRatio: BigDecimal =
        BigDecimal.apply("3.45455").setScale(5, RoundingMode.HALF_UP)
      val actualVoltRatio: BigDecimal =
        Transformer3wModel.voltRatio(transformer)

      actualVoltRatio shouldBe expectedVoltRatio
    }

    "should calculate the correct tap dependent voltage ratio - LV side" in new Transformer3wTestData {
      val transformer: Transformer3wModel = transformerModelLv
      val expectedVoltRatio: BigDecimal =
        BigDecimal.apply("19.0").setScale(5, RoundingMode.HALF_UP)
      val actualVoltRatio: BigDecimal =
        Transformer3wModel.voltRatio(transformer)

      actualVoltRatio shouldBe expectedVoltRatio
    }
  }
}
