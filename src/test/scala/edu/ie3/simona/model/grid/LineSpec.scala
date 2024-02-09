/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LineInputTestData
import edu.ie3.simona.test.common.model.grid.FiveLinesWithNodes
import edu.ie3.util.scala.OperationInterval
import squants.Each
import squants.electro.{Amperes, Kilovolts}
import squants.energy.Kilowatts

import java.util.UUID

/** Test class for [[LineModel]]
  */

class LineSpec extends UnitSpec with LineInputTestData {
  implicit val dimensionlessTolerance: squants.Dimensionless = Each(1e-12)
  implicit val electricCurrentTolerance: squants.electro.ElectricCurrent =
    Amperes(1e-12)

  sealed trait ValidLineModel {

    val validLineModel = new LineModel(
      UUID.fromString("38dd26c2-13e4-42d3-8aad-d92b55a7c7ba"),
      "validLineModel",
      OperationInterval(0L, 900L),
      UUID.fromString("6fc31563-61b5-4aa5-b623-09568c3a1c45"),
      UUID.fromString("e15bc358-1cc2-4025-9679-aa55f6b59428"),
      3,
      Amperes(300d),
      Each(0.0013109999999999999d),
      Each(0.0010680000000000002d),
      Each(0d),
      Each(0.60375d),
    )

  }

  def refSystem: RefSystem = {
    val nominalPower = Kilowatts(400d)
    val nominalVoltage = Kilovolts(10d)
    RefSystem(nominalPower, nominalVoltage)
  }

  "A valid LineInputModel" should {

    "be validated without an exception" in {
      LineModel.validateInputModel(lineInputMs10Kv)
    }

    "result in a valid LineModel" in {
      val validLineModel = LineModel(
        lineInputMs10Kv,
        refSystem,
        defaultSimulationStart,
        defaultSimulationEnd,
      )

      inside(validLineModel) {
        case LineModel(
              uuid,
              id,
              operationInterval,
              nodeAUuid,
              nodeBUuid,
              amount,
              iMax,
              r,
              x,
              g,
              b,
            ) =>
          uuid shouldBe lineInputMs10Kv.getUuid
          id shouldBe lineInputMs10Kv.getId
          operationInterval shouldBe defaultOperationInterval
          nodeAUuid shouldBe lineInputMs10Kv.getNodeA.getUuid
          nodeBUuid shouldBe lineInputMs10Kv.getNodeB.getUuid
          amount shouldBe lineInputMs10Kv.getParallelDevices
          iMax should approximate(
            Amperes(lineInputMs10Kv.getType.getiMax().getValue.doubleValue())
          )

          r should approximate(Each(0.0013109999999999999d))
          x should approximate(Each(0.0010680000000000002d))
          g should approximate(Each(0d))
          b should approximate(Each(0.00000060375d))
      }

      validLineModel.b0() should approximate(Each(0.000000301875d))
      validLineModel.bij() should approximate(Each(-373.5121155369499d))
      validLineModel.g0() should approximate(Each(0d))
      validLineModel.gij() should approximate(Each(458.4966137349637d))
    }

  }

  "An invalid LineInputModel" should {
    "not throw any exception, if the nodal voltage does not fit, but is lower than the line's rated voltage" in {
      noException shouldBe thrownBy {
        LineModel.validateInputModel(lineInputWithTooLowVoltLvlA)
        LineModel.validateInputModel(lineInputWithTooLowVoltLvlB)
      }
    }

    "throw a InvalidGridException if voltage of NodeA is higher than the line's nominal line voltage" in {
      val exception = intercept[InvalidGridException] {
        LineModel.validateInputModel(lineInputWithTooHighVoltLvlA)
      }

      exception.getMessage shouldBe s"Line ${lineInputWithTooHighVoltLvlA.getUuid} (${lineInputWithTooHighVoltLvlA.getId}) has a rated voltage of ${lineInputWithTooHighVoltLvlA.getType
          .getvRated()} but is connected to node A (${lineInputWithTooHighVoltLvlA.getNodeA.getUuid} / ${lineInputWithTooHighVoltLvlA.getNodeA.getId}), which has a rated voltage of ${lineInputWithTooHighVoltLvlA.getNodeA.getVoltLvl.getNominalVoltage}."
    }

    "throw a InvalidGridException if voltage of NodeB does not fit to its nominal line voltage" in {
      val exception = intercept[InvalidGridException] {
        LineModel.validateInputModel(lineInputWithTooHighVoltLvlB)
      }

      exception.getMessage shouldBe s"Line ${lineInputWithTooHighVoltLvlB.getUuid} (${lineInputWithTooHighVoltLvlB.getId}) has a rated voltage of ${lineInputWithTooHighVoltLvlB.getType
          .getvRated()} but is connected to node B (${lineInputWithTooHighVoltLvlB.getNodeB.getUuid} / ${lineInputWithTooHighVoltLvlB.getNodeB.getId}), which has a rated voltage of ${lineInputWithTooHighVoltLvlB.getNodeB.getVoltLvl.getNominalVoltage}."
    }

  }

  "A valid LineModel object" should {
    "calculate the branch admittance Y_ij of a given line model correctly" in new ValidLineModel {
      LineModel.yij(validLineModel) shouldBe Complex(
        1375.489841204891,
        -1120.5363466108497,
      )
    }

    "calculate the phase-to-ground admittance Y_m of a given line model correctly" in new ValidLineModel {
      LineModel.y0(validLineModel) shouldBe Complex(0, 0.905625)
    }

    "calculate the utilisation of a given line model correctly" in new ValidLineModel {

      val iNodeA: squants.electro.ElectricCurrent =
        Amperes(200d)
      val iNodeB: squants.electro.ElectricCurrent =
        Amperes(145d)

      LineModel.utilisation(validLineModel, iNodeA, iNodeB) should approximate(
        Each(22.222222222222218)
      )
    }

    "be able to be enabled and disabled on request" in new FiveLinesWithNodes {

      line0To3.isInOperation shouldBe false

      line0To3.enable()

      line0To3.isInOperation shouldBe true

      line0To3.disable()

      line0To3.isInOperation shouldBe false

      line0To3.enable()

      line0To3.isInOperation shouldBe true

    }

  }

}
