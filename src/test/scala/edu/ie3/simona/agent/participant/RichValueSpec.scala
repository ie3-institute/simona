/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import edu.ie3.datamodel.models.value.{
  HeatAndPValue,
  HeatAndSValue,
  HeatDemandValue,
  PValue,
  SValue,
  Value,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ActivePowerAndHeat,
  ComplexPower,
  ComplexPowerAndHeat,
  RichValue,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.Kilovars
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities

import scala.util.{Failure, Success}

class RichValueSpec extends UnitSpec with TableDrivenPropertyChecks {
  "A rich value" should {
    "fail on unsupported values" in {
      val maliciousValue = new HeatDemandValue(
        Quantities.getQuantity(12.5d, PowerSystemUnits.KILOWATT)
      )

      maliciousValue.toPrimaryData match {
        case Failure(exception) =>
          exception.getMessage shouldBe s"Cannot convert '$maliciousValue' to primary data."
        case Success(value) =>
          fail(
            s"Conversion from '$maliciousValue' to primary data was meant to fail, but succeeded with '$value'."
          )
      }
    }

    "fail on incomplete values" in {
      val table = Table(
        "maliciousValue",
        new PValue(null),
        new HeatAndPValue(
          null,
          Quantities.getQuantity(12.5, PowerSystemUnits.KILOWATT),
        ),
        new HeatAndPValue(
          Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT),
          null,
        ),
        new SValue(null, Quantities.getQuantity(25d, PowerSystemUnits.KILOVAR)),
        new SValue(
          Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT),
          null,
        ),
        new HeatAndSValue(
          null,
          Quantities.getQuantity(25d, PowerSystemUnits.KILOVAR),
          Quantities.getQuantity(12.5, PowerSystemUnits.KILOWATT),
        ),
        new HeatAndSValue(
          Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT),
          null,
          Quantities.getQuantity(12.5, PowerSystemUnits.KILOWATT),
        ),
        new HeatAndSValue(
          Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT),
          Quantities.getQuantity(25d, PowerSystemUnits.KILOVAR),
          null,
        ),
      )

      forAll(table) { (maliciousValue: Value) =>
        maliciousValue.toPrimaryData match {
          case Failure(exception) =>
            exception.getMessage shouldBe s"Cannot convert '$maliciousValue' to primary data."
          case Success(value) =>
            fail(
              s"Conversion from '$maliciousValue' to primary data was meant to fail, but succeeded with '$value'."
            )
        }
      }
    }

    "transfer supported values correctly to primary data" in {
      val table = Table(
        ("value", "primaryData"),
        (
          new PValue(Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT)),
          ActivePower(Kilowatts(50d)),
        ),
        (
          new HeatAndPValue(
            Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(12.5, PowerSystemUnits.KILOWATT),
          ),
          ActivePowerAndHeat(
            Kilowatts(50d),
            Kilowatts(12.5d),
          ),
        ),
        (
          new SValue(
            Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(25d, PowerSystemUnits.KILOVAR),
          ),
          ComplexPower(
            Kilowatts(50d),
            Kilovars(25d),
          ),
        ),
        (
          new HeatAndSValue(
            Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT),
            Quantities.getQuantity(25d, PowerSystemUnits.KILOVAR),
            Quantities.getQuantity(12.5, PowerSystemUnits.KILOWATT),
          ),
          ComplexPowerAndHeat(
            Kilowatts(50d),
            Kilovars(25d),
            Kilowatts(12.5),
          ),
        ),
      )

      forAll(table)({ case (value: Value, primaryData: PrimaryData) =>
        value.toPrimaryData match {
          case Success(actualPrimaryData) =>
            actualPrimaryData shouldBe primaryData
          case Failure(exception) =>
            fail(
              s"Conversion of '$value' to primary data should succeed, but failed.",
              exception,
            )
        }
      })
    }
  }
}
