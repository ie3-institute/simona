/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.util.scala.quantities
import edu.ie3.util.scala.quantities.ApparentPower.{
  PowerConversion,
  ReactivePowerConversion,
}
import edu.ie3.util.scala.quantities._
import edu.ie3.simona.test.matchers.SquantsMatchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.Angle
import squants.energy._
import squants.space.Degrees

class ApparentPowerSpec extends AnyFlatSpec with Matchers with SquantsMatchers {
  implicit val toleranceP: Power = Watts(1e-3)
  implicit val toleranceQ: ReactivePower = Vars(1e-3)
  implicit val toleranceAngle: Angle = Degrees(1e-3)

  behavior of "ApparentPower and its Units"

  it should "be applied correctly" in {
    val apparentPower = Voltampere(1, Degrees(0))
    apparentPower shouldBe Voltampere(Milliwatts(1000), Kilovars(0))

    apparentPower.value shouldBe 1.0
    apparentPower.angle should approximate(Degrees(0))
  }

  it should "be build from other power types" in {
    val fromPower = Watts(1000).asApparent(Kilovoltampere)
    fromPower.value shouldBe 1
    fromPower.angle shouldBe Degrees(0)

    val fromReactivePower = Millivars(1000).asApparent(Voltampere)
    fromReactivePower.value shouldBe 1
    fromReactivePower.angle should approximate(Degrees(90))
  }

  it should "return its parts correctly" in {
    val apparentPower = Voltampere(Watts(1))
    apparentPower.active shouldBe 1
    apparentPower.reactive shouldBe 0

    val updatedAngle = apparentPower.applyCosPhi(0.5)
    updatedAngle.active shouldBe 0.4999999999999999
    updatedAngle.reactive shouldBe 0.8660254037844387
  }

  it should "be converted to other units correctly" in {
    val voltAmpere = Voltampere(Kilowatts(100), Kilovars(1))

    val milli = voltAmpere.toMillivoltampere
    milli.activePower should approximate(Milliwatts(100000000))
    milli.reactivePower should approximate(Millivars(999999.9999999999))

    val kilo = voltAmpere.toKilovoltampere
    kilo.activePower should approximate(Kilowatts(100))
    kilo.reactivePower should approximate(Kilovars(1))

    val mega = voltAmpere.toMegavoltampere
    mega.activePower should approximate(Megawatts(0.1))
    mega.reactivePower should approximate(Megavars(0.001))

    val giga = voltAmpere.toGigavoltampere
    giga.activePower should approximate(Gigawatts(0.0001))
    giga.reactivePower should approximate(quantities.Gigavars(0.000001))
  }
}
