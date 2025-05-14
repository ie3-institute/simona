/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.system.{
  EvcsInput,
  LoadInput,
  PvInput,
  StorageInput,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.helper.TableDrivenHelper
import org.mockito.Mockito.when
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.Kilowatts

import java.util.UUID

class PrioritizedFlexStratSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with TableDrivenHelper
    with MockitoSugar {

  "The prioritized flex model" should {

    val load = UUID.fromString("0-0-0-0-1")
    val loadInputModel = mock[LoadInput]
    when(loadInputModel.getUuid).thenReturn(load)

    val pv = UUID.fromString("0-0-0-0-2")
    val pvInputModel = mock[PvInput]
    when(pvInputModel.getUuid).thenReturn(pv)

    val ev = UUID.fromString("0-0-0-0-3")
    val evcsInputModel = mock[EvcsInput]
    when(evcsInputModel.getUuid).thenReturn(ev)

    val st = UUID.fromString("0-0-0-0-4")
    val storageInputModel = mock[StorageInput]
    when(storageInputModel.getUuid).thenReturn(st)

    "determine flex control dependent on flex options" in {
      val strat = PrioritizedFlexStrat(curtailRegenerative = false)

      val cases = Table(
        (
          "target",
          "loadPower",
          "pvPower",
          "evcsSuggested",
          "evcsMin",
          "evcsMax",
          "storageMin",
          "storageMax",
          "expectedResult",
        ),

        /* excess feed-in */
        // excess is fully covered by parts of ev flexibility
        (0d, 0d, -5d, 2d, -11d, 11d, -2d, 2d, L((ev, 5d))),
        // excess is fully covered by maximum ev flexibility
        (0d, 0d, -11d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d))),
        // excess is fully covered by max ev and parts of storage flex
        (0d, 1d, -13d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d), (st, 1d))),
        // excess is fully covered by max ev and max storage flex
        (0d, 1d, -14d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d), (st, 2d))),
        // excess is fully covered by max storage flex
        (0d, 0d, -4d, 2d, 0d, 2d, -2d, 2d, L((st, 2d))),
        // excess is partly covered by max ev and max storage flex, -2kW remains
        (0d, 0d, -15d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d), (st, 2d))),
        // excess is partly covered by max storage flex, -7kW remains
        (0d, 0d, -14d, 2d, 0d, 2d, -5d, 5d, L((st, 5d))),
        // excess can't be covered because there is no flexibility
        (0d, 0d, -5d, 2d, 2d, 2d, 0d, 0d, Seq.empty),

        /* excess load */
        // excess is fully covered by parts of storage flex
        (0d, 5d, -1d, 0d, -11d, 11d, -5d, 5d, L((st, -4d))),
        // excess is fully covered by min storage flex
        (0d, 6d, -1d, 0d, -11d, 11d, -5d, 5d, L((st, -5d))),
        // excess is fully covered by min storage and parts of ev flex, charging power reduced
        (0d, 5d, -1d, 3d, -11d, 11d, -5d, 5d, L((st, -5d), (ev, 1d))),
        // excess is fully covered by min storage and parts of ev flex, vehicle-to-home
        (0d, 5d, -1d, 5d, -11d, 11d, -2d, 2d, L((st, -2d), (ev, -2d))),
        // excess is fully covered by min storage and min ev flex
        (0d, 14d, -1d, 5d, -11d, 11d, -2d, 2d, L((st, -2d), (ev, -11d))),
        // excess is fully covered by min ev flex
        (0d, 12d, -1d, 2d, -11d, 11d, 0d, 0d, L((ev, -11d))),
        // excess is partly covered by min ev and min storage flex, 1kW remains
        (0d, 15d, -1d, 2d, -11d, 11d, -2d, 2d, L((st, -2d), (ev, -11d))),
        // excess is partly covered by min ev flex, 2kW remains
        (0d, 14d, -1d, 4d, -11d, 11d, 0d, 0d, L((ev, -11d))),
        // excess can't be covered because there is no flexibility
        (0d, 5d, 0d, 2d, 2d, 2d, 0d, 0d, Seq.empty),

        /* target unequal to zero */
        // excess feed-in is fully covered by parts of ev flexibility
        (2d, 0d, -5d, 2d, -11d, 11d, -2d, 2d, L((ev, 7d))),
        // no excess
        (-3d, 0d, -5d, 2d, -11d, 11d, -2d, 2d, L.empty),
        // excess is fully covered by min storage and parts of ev flex, vehicle-to-home
        (-3d, 5d, -1d, 5d, -11d, 11d, -2d, 2d, L((st, -2d), (ev, -5d))),
      )

      forAll(cases) {
        (
            target,
            loadPower,
            pvPower,
            evcsSuggested,
            evcsMin,
            evcsMax,
            storageMin,
            storageMax,
            expectedResult,
        ) =>
          val flexOptions = Seq(
            (
              loadInputModel,
              MinMaxFlexOptions(
                Kilowatts(loadPower),
                Kilowatts(loadPower),
                Kilowatts(loadPower),
              ),
            ),
            (
              pvInputModel,
              MinMaxFlexOptions(
                Kilowatts(pvPower),
                Kilowatts(pvPower),
                Kilowatts(0d),
              ),
            ),
            (
              evcsInputModel,
              MinMaxFlexOptions(
                Kilowatts(evcsSuggested),
                Kilowatts(evcsMin),
                Kilowatts(evcsMax),
              ),
            ),
            (
              storageInputModel,
              MinMaxFlexOptions(
                Kilowatts(0d),
                Kilowatts(storageMin),
                Kilowatts(storageMax),
              ),
            ),
          )

          val actualResults =
            strat.determineFlexControl(
              flexOptions,
              Kilowatts(target),
            )

          actualResults should have size expectedResult.size withClue
            s"actual results are $actualResults while $expectedResult was expected."

          val expectedResultMap = expectedResult.toMap
          actualResults.foreach { case (uuid, power) =>
            val expectedRes: Double = expectedResultMap.getOrElse(
              uuid,
              fail(
                s"Actual control message $power for model $uuid is not part of the expected"
              ),
            )

            power.toKilowatts should ===(expectedRes +- 1e-6d)
          }
      }
    }

    "determine flex control dependent on flex options with curtailment enabled" in {
      val strat = PrioritizedFlexStrat(curtailRegenerative = true)

      val cases = Table(
        (
          "loadPower",
          "pvPower",
          "evcsSuggested",
          "evcsMin",
          "evcsMax",
          "storageMin",
          "storageMax",
          "expectedResult",
        ),

        /* excess feed-in */
        // excess is fully covered by parts of ev flexibility
        (0d, -5d, 2d, -11d, 11d, -2d, 2d, L((ev, 5d))),
        // excess is fully covered by maximum ev flexibility
        (0d, -11d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d))),
        // excess is fully covered by max ev and parts of storage flex
        (1d, -13d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d), (st, 1d))),
        // excess is fully covered by max ev and max storage flex, use pv 1kW flex
        (0d, -14d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d), (st, 2d), (pv, -13d))),
        // excess is fully covered by max storage flex
        (0d, -4d, 2d, 0d, 2d, -2d, 2d, L((st, 2d))),
        // excess is partly covered by max ev and max storage flex, use pv 2kW flex
        (0d, -15d, 2d, -11d, 11d, -2d, 2d, L((ev, 11d), (st, 2d), (pv, -13d))),
        // excess is partly covered by max storage flex, use 7kW pv flex
        (0d, -14d, 2d, 0d, 2d, -5d, 5d, L((st, 5d), (pv, -7d))),
        // excess can't be covered without using pv flex
        (0d, -5d, 2d, 2d, 2d, 0d, 0d, L((pv, -2d))),

        /* excess load, works same as above */
        // excess is fully covered by parts of storage flex
        (5d, -1d, 0d, -11d, 11d, -5d, 5d, L((st, -4d))),
        // excess is fully covered by min storage flex
        (6d, -1d, 0d, -11d, 11d, -5d, 5d, L((st, -5d))),
      )

      forAll(cases) {
        (
            loadPower,
            pvPower,
            evcsSuggested,
            evcsMin,
            evcsMax,
            storageMin,
            storageMax,
            expectedResult,
        ) =>
          val flexOptions = Seq(
            (
              loadInputModel,
              MinMaxFlexOptions(
                Kilowatts(loadPower),
                Kilowatts(loadPower),
                Kilowatts(loadPower),
              ),
            ),
            (
              pvInputModel,
              MinMaxFlexOptions(
                Kilowatts(pvPower),
                Kilowatts(pvPower),
                Kilowatts(0d),
              ),
            ),
            (
              evcsInputModel,
              MinMaxFlexOptions(
                Kilowatts(evcsSuggested),
                Kilowatts(evcsMin),
                Kilowatts(evcsMax),
              ),
            ),
            (
              storageInputModel,
              MinMaxFlexOptions(
                Kilowatts(0d),
                Kilowatts(storageMin),
                Kilowatts(storageMax),
              ),
            ),
          )

          val actualResults =
            strat.determineFlexControl(
              flexOptions,
              Kilowatts(0d),
            )

          actualResults should have size expectedResult.size withClue
            s"actual results are $actualResults while $expectedResult was expected."

          val expectedResultMap = expectedResult.toMap
          actualResults.foreach { case (uuid, power) =>
            val expectedRes: Double = expectedResultMap.getOrElse(
              uuid,
              fail(
                s"Actual control message $power for model $uuid is not part of the expected"
              ),
            )

            power.toKilowatts should ===(expectedRes +- 1e-6d)
          }
      }
    }

    "adapt flex options correctly" in {

      // flex options should be changed if corresponding
      // agent is not controlled by this strategy
      val cases = Table(
        ("curtailRegenerative", "inputModel", "expectedAdaptation"),
        (false, loadInputModel, true),
        (false, pvInputModel, true),
        (false, evcsInputModel, false),
        (false, storageInputModel, false),
        (true, loadInputModel, true),
        (true, pvInputModel, false),
        (true, evcsInputModel, false),
        (true, storageInputModel, false),
      )

      forAll(cases) {
        case (curtailRegenerative, inputModel, expectedAdaptation) =>
          val flexOptionsIn = MinMaxFlexOptions(
            Kilowatts(1),
            Kilowatts(-1),
            Kilowatts(2),
          )

          val flexOptionsOut = PrioritizedFlexStrat(curtailRegenerative)
            .adaptFlexOptions(inputModel, flexOptionsIn)

          if expectedAdaptation then {
            flexOptionsOut shouldBe MinMaxFlexOptions
              .noFlexOption(Kilowatts(1))
          } else {
            flexOptionsOut shouldBe flexOptionsIn
          }
      }
    }

  }
}
