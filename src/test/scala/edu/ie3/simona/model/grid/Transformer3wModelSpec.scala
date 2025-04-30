/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import edu.ie3.simona.exceptions.InvalidActionRequestException
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.{
  PowerFlowCaseA,
  PowerFlowCaseB,
  PowerFlowCaseC,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.Transformer3wTestData
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  Megavoltamperes,
  Voltamperes,
}
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor4}
import squants.Each
import tech.units.indriya.quantity.Quantities

import scala.math.BigDecimal.RoundingMode

class Transformer3wModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with Transformer3wTestData {
  val testingTolerance = 1e-5
  implicit val dimensionlessTolerance: squants.Dimensionless = Each(1e-8)
  implicit val powerTolerance: ApparentPower = Voltamperes(1e-3)

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
          transformer3wInput.isAutoTap,
        )

      val transformerModel: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemEhv,
          1,
          defaultSimulationStart,
          defaultSimulationEnd,
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
              sRated,
              r,
              x,
              g,
              b,
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
          sRated should approximate(Megavoltamperes(120))
          r should approximate(Each(1.03878e-3))
          x should approximate(Each(166.34349e-3))
          g should approximate(Each(1.874312e-6))
          b should approximate(Each(-75.012912e-6))
      }

      val yii: Complex = Transformer3wModel.y0(
        transformerModel,
        Transformer3wModel.Transformer3wPort.A,
      )
      yii shouldBe Complex.zero
      val yjj: Complex =
        Transformer3wModel.y0(
          transformerModel,
          Transformer3wModel.Transformer3wPort.INTERNAL,
        )
      implicit val doubleTolerance: Double = 1e-11
      yjj.real shouldBe 1.874312e-6 +- doubleTolerance
      yjj.imag shouldBe -75.012912e-6 +- doubleTolerance
      val yij: Complex = Transformer3wModel.yij(transformerModel)
      yij.real shouldBe 37.540107341e-3 +- doubleTolerance
      yij.imag shouldBe -6.011422522227 +- doubleTolerance
    }

    "result in a valid three winding transformer model - mv side" in new Transformer3wTestData {
      val expectedTappingModel: TransformerTappingModel =
        TransformerTappingModel(
          transformer3wInput.getType.getdV(),
          transformer3wInput.getTapPos,
          transformer3wInput.getType.getTapMax,
          transformer3wInput.getType.getTapMin,
          transformer3wInput.getType.getTapNeutr,
          transformer3wInput.isAutoTap,
        )

      val transformerModel: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd,
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
              sRated,
              r,
              x,
              g,
              b,
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
          sRated should approximate(Megavoltamperes(60))
          r should approximate(Each(240.9972299e-6))
          x should approximate(Each(24.99307479224e-3))
          g should approximate(Each(0d))
          b should approximate(Each(0d))
      }

      val yii: Complex = Transformer3wModel.y0(
        transformerModel,
        Transformer3wModel.Transformer3wPort.A,
      )
      yii shouldBe Complex.zero
      val yjj: Complex =
        Transformer3wModel.y0(
          transformerModel,
          Transformer3wModel.Transformer3wPort.INTERNAL,
        )
      yjj shouldBe Complex.zero

      val yij: Complex = Transformer3wModel.yij(transformerModel)
      implicit val doubleTolerance: Double = testingTolerance
      yij.real shouldBe 385.773e-3 +- doubleTolerance
      yij.imag shouldBe -40.007364 +- doubleTolerance
    }

    "result in a valid three winding transformer model - lv side" in new Transformer3wTestData {
      val expectedTappingModel: TransformerTappingModel =
        TransformerTappingModel(
          transformer3wInput.getType.getdV(),
          transformer3wInput.getTapPos,
          transformer3wInput.getType.getTapMax,
          transformer3wInput.getType.getTapMin,
          transformer3wInput.getType.getTapNeutr,
          transformer3wInput.isAutoTap,
        )

      val transformerModel: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd,
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
              sRated,
              r,
              x,
              g,
              b,
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
          sRated should approximate(Megavoltamperes(40))
          r should approximate(Each(3.185595567e-6))
          x should approximate(Each(556.0941828e-6))
          g should approximate(Each(0d))
          b should approximate(Each(0d))
      }

      val yii: Complex = Transformer3wModel.y0(
        transformerModel,
        Transformer3wModel.Transformer3wPort.A,
      )
      yii shouldBe Complex.zero
      val yjj: Complex =
        Transformer3wModel.y0(
          transformerModel,
          Transformer3wModel.Transformer3wPort.INTERNAL,
        )
      yjj shouldBe Complex.zero
      val yij: Complex = Transformer3wModel.yij(transformerModel)
      implicit val doubleTolerance: Double = testingTolerance
      yij.real shouldBe 10.301007 +- doubleTolerance
      yij.imag shouldBe -1798.197528 +- doubleTolerance
    }

    "result in an enabled three winding transformer model, if the input model is enabled at simulation start" in new Transformer3wTestData {
      val transformerModelEhvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemEhv,
          1,
          defaultSimulationStart,
          defaultSimulationEnd,
        )
      val transformerModelHvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd,
        )
      val transformerModelLvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInput,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd,
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
          defaultSimulationEnd,
        )
      val transformerModelHvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputPostponed,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd,
        )
      val transformerModelLvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputPostponed,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd,
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
          defaultSimulationEnd,
        )
      val transformerModelHvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputTapped,
          mainRefSystemHv,
          2,
          defaultSimulationStart,
          defaultSimulationEnd,
        )
      val transformerModelLvTemp: Transformer3wModel =
        Transformer3wModel(
          transformer3wInputTapped,
          mainRefSystemLv,
          3,
          defaultSimulationStart,
          defaultSimulationEnd,
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
          defaultSimulationEnd,
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
            -2,
          ), /* Limit to min tap (should be -3 limited to -2) */
        )

      val transformerModel: Transformer3wModel = transformerModelEhv

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
            yjjExpected: Complex,
        ) =>
          {
            transformer.updateTapPos(tapPos)
            val yijActual = Transformer3wModel.yij(transformer)
            val yiiActual = Transformer3wModel.y0(
              transformer,
              Transformer3wModel.Transformer3wPort.A,
            )
            val yjjActual =
              Transformer3wModel.y0(
                transformer,
                Transformer3wModel.Transformer3wPort.INTERNAL,
              )

            /* Remark: This is not really precise. At the moment, double-based calculations do
             * hinder us from being more precise. Maybe it is advisory to switch over to BigDecimal */
            implicit val doubleTolerance: Double = 1e-4
            yijActual.real shouldBe yijExpected.real +- doubleTolerance
            yijActual.imag shouldBe yijExpected.imag +- doubleTolerance

            yiiActual.real shouldBe yiiExpected.real +- doubleTolerance
            yiiActual.imag shouldBe yiiExpected.imag +- doubleTolerance

            yjjActual.real shouldBe yjjExpected.real +- doubleTolerance
            yjjActual.imag shouldBe yjjExpected.imag +- doubleTolerance
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
