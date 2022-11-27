/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.helper.TableDrivenHelper
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar

import java.util.UUID

class ProportionalFlexStratSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with TableDrivenHelper
    with MockitoSugar {

  "The proportional flex model" should {

    "determine flex control dependent on flex options" in {

      val spi = mock[SystemParticipantInput] // is not used

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
          "expected2"
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
        (-4d, -1d, -2d, 2d, 1d, -3d, 0d, S(-1.8d), S(-2.2d)),
        (-4d, -2d, -2d, 0d, 0d, -4d, 0d, N, S(-2d)),
        (-4d, 0d, -4d, 0d, -2d, -2d, 0d, S(-2d), N),
        // target lower than min sum
        (-7d, -1d, -2d, 0d, 0d, -4d, 0d, S(-2d), S(-4d)),
        (-7d, -2d, -2d, 0d, 0d, -4d, 0d, N, S(-4d)),
        (-7d, 0d, -4d, 0d, -2d, -2d, 0d, S(-4d), N)
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
            expected2
        ) =>
          val flexOptions1 = ProvideMinMaxFlexOptions(
            modelUuid = UUID.randomUUID(),
            referencePower = ref1.asKiloWatt,
            minPower = min1.asKiloWatt,
            maxPower = max1.asKiloWatt
          )

          val flexOptions2 = ProvideMinMaxFlexOptions(
            modelUuid = UUID.randomUUID(),
            referencePower = ref2.asKiloWatt,
            minPower = min2.asKiloWatt,
            maxPower = max2.asKiloWatt
          )

          val actualResults = ProportionalFlexStrat
            .determineDeviceControl(
              Seq(
                (spi, flexOptions1),
                (spi, flexOptions2)
              ),
              target.asKiloWatt
            )
            .toMap

          actualResults should have size Seq(expected1, expected2).flatten.size

          expected1.foreach { exp1 =>
            actualResults.getOrElse(
              flexOptions1.modelUuid,
              fail(
                "Results should include a set point for device 1, but doesn't"
              )
            ) should beEquivalentTo(
              exp1.asKiloWatt
            )
          }

          expected2.foreach { exp2 =>
            actualResults.getOrElse(
              flexOptions2.modelUuid,
              fail(
                "Results should include a set point for device 2, but doesn't"
              )
            ) should beEquivalentTo(
              exp2.asKiloWatt
            )
          }

      }

    }
  }
}
