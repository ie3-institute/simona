/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.assets.control

import edu.ie3.datamodel.models.input.system.characteristic
import edu.ie3.datamodel.models.input.system.characteristic.{
  CharacteristicPoint,
  CosPhiP => CosPhiPInput,
  QV => QVInput,
}
import edu.ie3.simona.exceptions.QControlException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.control.QControl.{
  CosPhiFixed,
  CosPhiP,
  QV,
}
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower}
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import squants.Each
import tech.units.indriya.quantity.Quantities._

import java.util
import javax.measure.quantity.Dimensionless
import scala.collection.immutable.TreeSet

class QControlSpec extends UnitSpec with TableDrivenPropertyChecks {

  private val defaultTolerance = 1e-12
  private implicit val reactivePowerTolerance: ReactivePower = Megavars(1e-12)
  private implicit val tolerance: squants.Dimensionless = Each(1e-12)

  val validCosPhiPInput: characteristic.CosPhiP = new CosPhiPInput(
    "cosPhiP:{(0.0,-1.0),(0.5,-0.8),(1.0,-0.2)}"
  )

  val validQVInput = new QVInput(
    "qV:{(0.9,-1.0),(0.95,0.0),(1.05,0.0),(1.1,1.0)}"
  )

  def createXYPair(
      d1: Double,
      d2: Double,
  ): XYPair[squants.Dimensionless, squants.Dimensionless] = {
    XYPair(Each(d1), Each(d2))
  }

  "A valid QControl object" should {
    "throw an exception, if the provided fixed power factor input has more than one coordinate" in {
      val points =
        new util.TreeSet[CharacteristicPoint[Dimensionless, Dimensionless]](
          util.Arrays.asList(
            new CharacteristicPoint[Dimensionless, Dimensionless](
              getQuantity(1d, PU),
              getQuantity(2d, PU),
            ),
            new CharacteristicPoint[Dimensionless, Dimensionless](
              getQuantity(3d, PU),
              getQuantity(4d, PU),
            ),
          )
        )
      val invalidInput =
        new edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed(
          points
        )
      intercept[QControlException](
        QControl(invalidInput)
      ).getMessage shouldBe "Got an invalid definition of fixed power factor: cosPhiFixed{points=[CharacteristicCoordinate{x=1.0 p.u., y=2.0 p.u.}, CharacteristicCoordinate{x=3.0 p.u., y=4.0 p.u.}]}. It may only contain one coordinate"
    }

    "parse a valid CosPhiFixed correctly" in {
      val invalidInput =
        new edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed(
          "cosPhiFixed:{(0.0,0.9)}"
        )
      QControl(invalidInput) shouldBe CosPhiFixed(0.9)
    }

    "parse a valid CosPhiP correctly" in {
      QControl(validCosPhiPInput) shouldBe CosPhiP(
        TreeSet.from(
          Vector(
            createXYPair(0.0, -1.0),
            createXYPair(0.5, -0.8),
            createXYPair(1.0, -0.2),
          )
        )
      )
    }

    "parse a valid QV correctly" in {
      QControl(validQVInput) shouldBe QV(
        TreeSet.from(
          Vector(
            createXYPair(0.9, -1.0),
            createXYPair(0.95, 0.0),
            createXYPair(1.05, 0.0),
            createXYPair(1.1, 1.0),
          )
        )
      )
    }
  }

  "A valid cosphi_p regulation" should {
    val validCosPhiP = QControl(validCosPhiPInput) match {
      case cosPhiP: CosPhiP => cosPhiP
      case _                => fail()
    }

    "provide correct values when the requested value is part of the containing xy coordinates" in {
      val requestedValue = Each(0.5)

      validCosPhiP.cosPhi(requestedValue) should approximate(Each(-0.8))
    }

    "provide an interpolated value when the requested value is not part of the containing xy coordinates" in {
      val requestedValue = Each(0.75)

      validCosPhiP.cosPhi(requestedValue) should approximate(Each(-0.5))
    }

    "provide the last known value when the requested value is outside of the containing xy coordinates" in {
      validCosPhiP.cosPhi(Each(2.0)) should approximate(Each(-0.2))
      validCosPhiP.cosPhi(Each(-1.0)) should approximate(Each(-1.0))
    }
  }

  "A valid Q(V) control" should {
    val qMax = Megavars(250.0)

    "return correct reactive power for a linear function" in {
      val validQV =
        QControl(new QVInput("qV:{(0.95,-0.625),(1.05,0.625)}")) match {
          case qv: QV => qv
          case failed => fail("Got wrong q control instance'" + failed + "'.")
        }

      val testingPoints: TableFor2[Double, Double] = Table(
        ("v", "scaleExpected"),
        (0.90, -0.625),
        (0.95, -0.625),
        (0.96, -0.5),
        (0.97, -0.375),
        (0.98, -0.25),
        (0.99, -0.125),
        (1.0, 0.0),
        (1.01, 0.125),
        (1.02, 0.25),
        (1.03, 0.375),
        (1.04, 0.5),
        (1.05, 0.625),
        (1.10, 0.625),
      )

      forAll(testingPoints) { (v: Double, scaleExpected: Double) =>
        validQV.q(Each(v), qMax) should approximate(qMax * scaleExpected)
      }
    }

    "return correct reactive power for a piecewise function" in {
      val validQV = QControl(validQVInput) match {
        case qv: QV => qv
        case failed => fail("Got wrong q control instance'" + failed + "'.")
      }

      val testingPoints: TableFor2[Double, Double] = Table(
        ("v", "scaleExpected"),
        (0.88, -1.0),
        (0.9, -1),
        (0.91, -0.8),
        (0.92, -0.6),
        (0.93, -0.4),
        (0.94, -0.2),
        (0.95, 0.0),
        (0.96, 0.0),
        (0.97, 0.0),
        (0.98, 0.0),
        (0.99, 0.0),
        (1.0, 0.0),
        (1.01, 0.0),
        (1.02, 0.0),
        (1.03, 0.0),
        (1.04, 0.0),
        (1.05, 0.0),
        (1.06, 0.2),
        (1.07, 0.4),
        (1.08, 0.6),
        (1.09, 0.8),
        (1.1, 1.0),
        (1.12, 1.0),
      )

      forAll(testingPoints) { (v: Double, scaleExpected: Double) =>
        validQV.q(Each(v), qMax) should approximate(qMax * scaleExpected)
      }
    }
  }
}
