/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.helper.TableDrivenHelper
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar
import squants.Power
import squants.energy.{Kilowatts, Watts}

import java.util.UUID

class ProportionalFlexStratSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with TableDrivenHelper
    with MockitoSugar {

  private implicit val powerTolerance: Power = Watts(0.1)

  "The proportional flex model" should {

    "determine flex control dependent on flex options" in {

      val assetInput = mock[AssetInput] // is not used

      /* As an example, this is how the test cases should be interpreted,
       * using the second test case of target being higher than reference sum:
       *
       * We have a target of 3 kW, but a reference power of 1 kW + 0 kW = 1 kW.
       * Thus, we want to increase load by 2 kW.
       *
       * In total, we have a maximum power of 2 kW + 4 kW = 6 kW. This is 5 kW
       * more than our reference power of 1 kW. Since we need only 2 kW of the
       * 5 kW potential, we use 40% of the available positive flexibility.
       *
       * Specifically, we use 40% of the flex potential 2 kW - 1 kW = 1 kW of
       * the first unit and 40% of the flex potential 4 kW - 0 kW = 4 kW of the
       * second unit. Thus, we arrive at 1 kW + 40% * 1 kW = 1.4 kW for the
       * first unit and 0 kW + 40% * 4 kW = 1.6 kW for the second unit.
       */

      val cases = Table(
        (
          "target",
          "ref1",
          "min1",
          "max1",
          "ref2",
          "min2",
          "max2",
          "expected1",
          "expected2",
        ),

        /* target equal to reference sum */
        (4d, 2d, 0d, 3d, 2d, 0d, 4d, N, N),
        (2d, 1d, 0d, 1d, 1d, 0d, 4d, N, N),
        (2d, 1d, 0d, 1d, 1d, 0d, 1d, N, N),
        (-5d, -2d, -4d, 0d, -3d, -5d, 1d, N, N),
        (-2d, -1d, -1d, 1d, -1d, -2d, 0d, N, N),
        (-2d, -1d, -1d, 0d, -1d, -1d, 0d, N, N),

        /* target higher than reference sum */
        // target lower than max sum
        (5d, 1d, 0d, 2d, 0d, 0d, 4d, S(1.8d), S(3.2d)),
        (3d, 1d, 0d, 2d, 0d, 0d, 4d, S(1.4d), S(1.6d)),
        (2d, -1d, -1d, 2d, -1d, -1d, 4d, S(0.5d), S(1.5d)),
        (4d, 2d, 0d, 2d, 0d, 0d, 4d, N, S(2d)),
        (4d, 0d, 0d, 4d, 2d, 0d, 2d, S(2d), N),
        // target higher than max sum
        (7d, 1d, 0d, 2d, 0d, 0d, 4d, S(2d), S(4d)),
        (7d, 2d, 0d, 2d, 0d, 0d, 4d, N, S(4d)),
        (7d, 0d, 0d, 4d, 2d, 0d, 2d, S(4d), N),

        /* target lower than reference sum */
        // target higher than min sum
        (-1d, 1d, -1d, 2d, 0d, -2d, 0d, S(0d), S(-1d)),
        (-2d, 1d, -1d, 2d, 0d, -2d, 0d, S(-0.5d), S(-1.5d)),
        (-4d, -1d, -2d, 2d, 1d, -3d, 1d, S(-1.8d), S(-2.2d)),
        (-4d, -2d, -2d, 0d, 0d, -4d, 0d, N, S(-2d)),
        (-4d, 0d, -4d, 0d, -2d, -2d, 0d, S(-2d), N),
        // target lower than min sum
        (-7d, -1d, -2d, 0d, 0d, -4d, 0d, S(-2d), S(-4d)),
        (-7d, -2d, -2d, 0d, 0d, -4d, 0d, N, S(-4d)),
        (-7d, 0d, -4d, 0d, -2d, -2d, 0d, S(-4d), N),
      )

      forAll(cases) {
        (
            target,
            ref1,
            min1,
            max1,
            ref2,
            min2,
            max2,
            expected1,
            expected2,
        ) =>
          val flexOptions1 = ProvideMinMaxFlexOptions(
            modelUuid = UUID.randomUUID(),
            ref = Kilowatts(ref1),
            min = Kilowatts(min1),
            max = Kilowatts(max1),
          )

          val flexOptions2 = ProvideMinMaxFlexOptions(
            modelUuid = UUID.randomUUID(),
            ref = Kilowatts(ref2),
            min = Kilowatts(min2),
            max = Kilowatts(max2),
          )

          val actualResults = ProportionalFlexStrat
            .determineFlexControl(
              Seq(
                (assetInput, flexOptions1),
                (assetInput, flexOptions2),
              ),
              Kilowatts(target),
            )
            .toMap

          actualResults should have size Seq(expected1, expected2).flatten.size

          expected1.foreach { exp1 =>
            val res1 = actualResults.getOrElse(
              flexOptions1.modelUuid,
              fail(
                "Results should include a set point for device 1, but doesn't"
              ),
            )
            (res1 ~= Kilowatts(exp1)) shouldBe true
          }

          expected2.foreach { exp2 =>
            val res2 = actualResults.getOrElse(
              flexOptions2.modelUuid,
              fail(
                "Results should include a set point for device 2, but doesn't"
              ),
            )
            (res2 ~= Kilowatts(exp2)) shouldBe true
          }

      }
    }

    "adapt flex options correctly" in {
      val assetInput = mock[AssetInput] // is not used

      val flexOptionsIn = ProvideMinMaxFlexOptions(
        UUID.randomUUID(),
        Kilowatts(1),
        Kilowatts(-1),
        Kilowatts(2),
      )

      val flexOptionsOut =
        ProportionalFlexStrat.adaptFlexOptions(assetInput, flexOptionsIn)

      flexOptionsOut shouldBe flexOptionsIn
    }
  }
}
