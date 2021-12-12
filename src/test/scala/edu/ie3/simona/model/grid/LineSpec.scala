/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import java.util.UUID

import breeze.math.Complex
import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LineInputTestData
import edu.ie3.simona.test.common.model.grid.FiveLinesWithNodes
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.OperationInterval
import javax.measure.quantity.ElectricCurrent
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

/** Test class for [[LineModel]]
  */
class LineSpec extends UnitSpec with LineInputTestData {

  sealed trait ValidLineModel {

    val validLineModel = new LineModel(
      UUID.fromString("38dd26c2-13e4-42d3-8aad-d92b55a7c7ba"),
      "validLineModel",
      OperationInterval(0L, 900L),
      UUID.fromString("6fc31563-61b5-4aa5-b623-09568c3a1c45"),
      UUID.fromString("e15bc358-1cc2-4025-9679-aa55f6b59428"),
      3,
      Quantities.getQuantity(300, AMPERE),
      Quantities.getQuantity(0.0013109999999999999, PU),
      Quantities.getQuantity(0.0010680000000000002, PU),
      Quantities.getQuantity(0, PU),
      Quantities.getQuantity(0.60375, PU)
    )

  }

  def refSystem: RefSystem = {
    val nominalPower = Quantities.getQuantity(400, KILOVOLTAMPERE)
    val nominalVoltage = Quantities.getQuantity(10, KILOVOLT)
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
        defaultSimulationEnd
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
              b
            ) =>
          uuid shouldBe lineInputMs10Kv.getUuid
          id shouldBe lineInputMs10Kv.getId
          operationInterval shouldBe defaultOperationInterval
          nodeAUuid shouldBe lineInputMs10Kv.getNodeA.getUuid
          nodeBUuid shouldBe lineInputMs10Kv.getNodeB.getUuid
          amount shouldBe lineInputMs10Kv.getParallelDevices
          iMax shouldBe lineInputMs10Kv.getType.getiMax()

          r should equalWithTolerance(
            Quantities.getQuantity(0.0013109999999999999, PU)
          )
          x should equalWithTolerance(
            Quantities.getQuantity(0.0010680000000000002, PU)
          )
          g should equalWithTolerance(Quantities.getQuantity(0, PU))
          b should equalWithTolerance(Quantities.getQuantity(0.00000060375, PU))
      }

      validLineModel.b0() should equalWithTolerance(
        Quantities.getQuantity(0.000000301875, PU)
      )
      validLineModel.bij() should equalWithTolerance(
        Quantities.getQuantity(-373.5121155369499, PU)
      )
      validLineModel.g0() should equalWithTolerance(
        Quantities.getQuantity(0, PU)
      )
      validLineModel.gij() should equalWithTolerance(
        Quantities.getQuantity(458.4966137349637, PU)
      )
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
        -1120.5363466108497
      )
    }

    "calculate the phase-to-ground admittance Y_m of a given line model correctly" in new ValidLineModel {
      LineModel.y0(validLineModel) shouldBe Complex(0, 0.905625)
    }

    "calculate the utilisation of a given line model correctly" in new ValidLineModel {

      val iNodeA: ComparableQuantity[ElectricCurrent] =
        Quantities.getQuantity(200, AMPERE)
      val iNodeB: ComparableQuantity[ElectricCurrent] =
        Quantities.getQuantity(145, AMPERE)

      LineModel.utilisation(validLineModel, iNodeA, iNodeB) shouldBe Quantities
        .getQuantity(22.222222222222218, PERCENT)

    }

    "be able to be enabled and disabled on request" in new FiveLinesWithNodes {

      line03.isInOperation shouldBe false

      line03.enable()

      line03.isInOperation shouldBe true

      line03.disable()

      line03.isInOperation shouldBe false

      line03.enable()

      line03.isInOperation shouldBe true

    }

  }

}
