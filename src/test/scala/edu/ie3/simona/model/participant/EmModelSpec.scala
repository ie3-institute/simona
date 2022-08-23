/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.{
  EvcsInput,
  LoadInput,
  PvInput,
  StorageInput
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.EmInputTestData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.mockito.Mockito.when
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar.mock

import java.util.UUID

class EmModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with EmInputTestData {

  "The em model object" should {

    "determine flex" in {
      val model = EmModel(
        emInputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
      )

      val load = UUID.randomUUID()
      val loadInputModel = mock[LoadInput]
      when(loadInputModel.getUuid).thenReturn(load)

      val pv = UUID.randomUUID()
      val pvInputModel = mock[PvInput]
      when(pvInputModel.getUuid).thenReturn(pv)

      val evcs = UUID.randomUUID()
      val evcsInputModel = mock[EvcsInput]
      when(evcsInputModel.getUuid).thenReturn(evcs)

      val strg = UUID.randomUUID()
      val storageInputModel = mock[StorageInput]
      when(storageInputModel.getUuid).thenReturn(strg)

      val cases = Table(
        (
          "loadPower",
          "pvPower",
          "evcsSuggested",
          "evcsMin",
          "evcsMax",
          "storageMin",
          "storageMax",
          "expectedResult"
        ),
        // TODO add explanation to each row
        /* excess feed-in */
        // excess is fully covered by parts of evcs flexibility
        (0d, -5d, 2d, -11d, 11d, -2d, 2d, Seq((evcs, 5d))),
        // excess is fully covered by maximum evcs flexibility
        (0d, -11d, 2d, -11d, 11d, -2d, 2d, Seq((evcs, 11d))),
        // excess is fully covered by max evcs and parts of storage flex
        (1d, -13d, 2d, -11d, 11d, -2d, 2d, Seq((evcs, 11d), (strg, 1d))),
        // excess is fully covered by max evcs and max storage flex
        (0d, -14d, 2d, -11d, 11d, -2d, 2d, Seq((evcs, 11d), (strg, 2d))),
        // excess is fully covered by max storage flex
        (0d, -4d, 2d, 0d, 2d, -2d, 2d, Seq((strg, 2d))),
        // excess is partly covered by max evcs and max storage flex, -2kW remains
        (0d, -15d, 2d, -11d, 11d, -2d, 2d, Seq((evcs, 11d), (strg, 2d))),
        // excess is partly covered by max storage flex, -7kW remains
        (0d, -14d, 2d, 0d, 2d, -5d, 5d, Seq((strg, 5d))),
        // excess can't be covered because there is no flexibility
        (0d, -5d, 2d, 2d, 2d, 0d, 0d, Seq.empty),
        /* excess load */
        // excess is fully covered by parts of storage flex
        (5d, -1d, 0d, -11d, 11d, -5d, 5d, Seq((strg, -4d))),
        // excess is fully covered by min storage flex
        (6d, -1d, 0d, -11d, 11d, -5d, 5d, Seq((strg, -5d))),
        // excess is fully covered by min storage and parts of evcs flex, charging power reduced
        (5d, -1d, 3d, -11d, 11d, -5d, 5d, Seq((strg, -5d), (evcs, 1d))),
        // excess is fully covered by min storage and parts of evcs flex, vehicle-to-home
        (5d, -1d, 5d, -11d, 11d, -2d, 2d, Seq((strg, -2d), (evcs, -2d))),
        // excess is fully covered by min storage and min evcs flex
        (14d, -1d, 5d, -11d, 11d, -2d, 2d, Seq((strg, -2d), (evcs, -11d))),
        // excess is fully covered by min evcs flex
        (12d, -1d, 2d, -11d, 11d, 0d, 0d, Seq((evcs, -11d))),
        // excess is partly covered by min evcs and min storage flex, 1kW remains
        (15d, -1d, 2d, -11d, 11d, -2d, 2d, Seq((strg, -2d), (evcs, -11d))),
        // excess is partly covered by min evcs flex, 2kW remains
        (14d, -1d, 4d, -11d, 11d, 0d, 0d, Seq((evcs, -11d))),
        // excess can't be covered because there is no flexibility
        (5d, 0d, 2d, 2d, 2d, 0d, 0d, Seq.empty)
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
            expectedResult
        ) =>
          val flexOptions = Seq(
            (
              loadInputModel,
              ProvideMinMaxFlexOptions(
                load,
                loadPower.asKiloWatt,
                loadPower.asKiloWatt,
                loadPower.asKiloWatt
              )
            ),
            (
              pvInputModel,
              ProvideMinMaxFlexOptions(
                pv,
                pvPower.asKiloWatt,
                pvPower.asKiloWatt,
                0d.asKiloWatt
              )
            ),
            (
              evcsInputModel,
              ProvideMinMaxFlexOptions(
                evcs,
                evcsSuggested.asKiloWatt,
                evcsMin.asKiloWatt,
                evcsMax.asKiloWatt
              )
            ),
            (
              storageInputModel,
              ProvideMinMaxFlexOptions(
                strg,
                0d.asKiloWatt,
                storageMin.asKiloWatt,
                storageMax.asKiloWatt
              )
            )
          )

          val actualResults = model.determineDeviceControl(flexOptions)

          actualResults should have size expectedResult.size

          val expectedResultMap = expectedResult.toMap
          actualResults.foreach { case (uuid, msg) =>
            val expectedRes: Double = expectedResultMap.getOrElse(
              uuid,
              fail(
                s"Actual control message $msg for model $uuid is not part of the expected"
              )
            )

            msg.power
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue() should ===(expectedRes +- 1e-6d)
          }
      }
    }
  }

}
