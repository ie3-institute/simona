/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs.gridoriented

import edu.ie3.simona.model.participant.evcs.PredictionAndSchedulingUtils.TimeStamp
import edu.ie3.simona.model.participant.evcs.gridoriented.VoltagePrediction
import edu.ie3.simona.model.participant.evcs.gridoriented.VoltagePrediction.VoltageTimeTableEntry
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.DayOfWeek
import javax.measure.quantity.{Dimensionless, Length}
import scala.util.Try

class VoltagePredictionSpec extends UnitSpec {
  "Blurring voltages" when {

    "having a proper input set" should {
      val windowLength = 3
      val input = Range(0, 5).map(cnt =>
        (
          TimeStamp(DayOfWeek.MONDAY, cnt, 0),
          Quantities.getQuantity(cnt, PowerSystemUnits.PU)
        )
      )

      "lead to proper enhanced values" in {
        val enhanceVoltageReferences = PrivateMethod[Try[
          Seq[(TimeStamp, ComparableQuantity[Dimensionless])]
        ]](Symbol("enhanceVoltageReferences"))

        val actual = VoltagePrediction invokePrivate enhanceVoltageReferences(
          input,
          windowLength
        )
        val result = actual.success.get

        result.size shouldBe input.length + 2
        result.map(_._1) should contain theSameElementsInOrderAs Seq(4, 0, 1, 2,
          3, 4, 0).map(cnt => TimeStamp(DayOfWeek.MONDAY, cnt, 0))
      }

      "reveal correct blurred values" in {
        val blurVoltages =
          PrivateMethod[Try[Seq[VoltageTimeTableEntry]]](Symbol("blurVoltages"))

        val expectedQuantities =
          Seq(
            Quantities.getQuantity(1.6667d, PowerSystemUnits.PU),
            Quantities.getQuantity(1d, PowerSystemUnits.PU),
            Quantities.getQuantity(2d, PowerSystemUnits.PU),
            Quantities.getQuantity(3d, PowerSystemUnits.PU),
            Quantities.getQuantity(2.3333d, PowerSystemUnits.PU)
          )

        val actual =
          VoltagePrediction invokePrivate blurVoltages(input, windowLength)
        val result = actual.success.get

        result.size shouldBe expectedQuantities.length
        result.zip(expectedQuantities).foreach {
          case (VoltageTimeTableEntry(_, _, actualVoltage), expectedVoltage) =>
            actualVoltage should equalWithTolerance(expectedVoltage, 1e-3)
        }
      }
    }
  }

  "Determining the average of quantities" should {
    val mean = PrivateMethod[Option[ComparableQuantity[Length]]](Symbol("mean"))

    "lead to correct results" in {
      val input = Seq(1, 3, 5).map { value =>
        Quantities.getQuantity(value, Units.METRE)
      }

      val actual = VoltagePrediction invokePrivate mean(input)
      actual match {
        case Some(value) =>
          value should equalWithTolerance(
            Quantities.getQuantity(3, Units.METRE)
          )
        case None => fail("Unable to determine mean of quantities.")
      }
    }

    "return empty optional, if no quantities are provided" in {
      (VoltagePrediction invokePrivate mean(
        Seq.empty[ComparableQuantity[Length]]
      )) shouldBe empty
    }
  }
}
