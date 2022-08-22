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
import org.mockito.Mockito.when
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar.mock
import tech.units.indriya.quantity.Quantities

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

      val loadUuid = UUID.randomUUID()
      val loadInputModel = mock[LoadInput]
      when(loadInputModel.getUuid).thenReturn(loadUuid)

      val pvUuid = UUID.randomUUID()
      val pvInputModel = mock[PvInput]
      when(pvInputModel.getUuid).thenReturn(pvUuid)

      val evcsUuid = UUID.randomUUID()
      val evcsInputModel = mock[EvcsInput]
      when(evcsInputModel.getUuid).thenReturn(evcsUuid)

      val storageUuid = UUID.randomUUID()
      val storageInputModel = mock[StorageInput]
      when(storageInputModel.getUuid).thenReturn(storageUuid)

      val cases = Table(
        (
          "loadPower",
          "pvPower",
          "evcsSuggested",
          "evcsSRated",
          "storageSRated",
          "expectedResult"
        ),
        // TODO add explanation to each row
        // excess feed-in
        (0d, -5d, 2d, 11d, 2d, Seq((evcsUuid, 5d))),
        (1d, -13d, 2d, 11d, 2d, Seq((evcsUuid, 11d), (storageUuid, 1d))),
        (0d, -14d, 2d, 11d, 2d, Seq((evcsUuid, 11d), (storageUuid, 2d))),
        // (0d, -14d, 2d, 0d, 5d, Seq((storageUuid, 5d))), FIXME
        // excess load
        (5d, -1d, 0d, 11d, 5d, Seq((storageUuid, -4d))),
        (5d, -1d, 3d, 11d, 5d, Seq((storageUuid, -5d), (evcsUuid, 1d))),
        (5d, -1d, 5d, 11d, 2d, Seq((storageUuid, -2d), (evcsUuid, -2d)))
      )

      forAll(cases) {
        (
            loadPower,
            pvPower,
            evcsSuggested,
            evcsSRated,
            storageSRated,
            expectedResult
        ) =>
          val flexOptions = Seq(
            (
              loadInputModel,
              ProvideMinMaxFlexOptions(
                loadUuid,
                Quantities.getQuantity(loadPower, PowerSystemUnits.KILOWATT),
                Quantities.getQuantity(loadPower, PowerSystemUnits.KILOWATT),
                Quantities.getQuantity(loadPower, PowerSystemUnits.KILOWATT)
              )
            ),
            (
              pvInputModel,
              ProvideMinMaxFlexOptions(
                pvUuid,
                Quantities.getQuantity(pvPower, PowerSystemUnits.KILOWATT),
                Quantities.getQuantity(pvPower, PowerSystemUnits.KILOWATT),
                Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT)
              )
            ),
            (
              evcsInputModel,
              ProvideMinMaxFlexOptions(
                evcsUuid,
                Quantities
                  .getQuantity(evcsSuggested, PowerSystemUnits.KILOWATT),
                Quantities
                  .getQuantity(evcsSRated * -1d, PowerSystemUnits.KILOWATT),
                Quantities.getQuantity(evcsSRated, PowerSystemUnits.KILOWATT)
              )
            ),
            (
              storageInputModel,
              ProvideMinMaxFlexOptions(
                storageUuid,
                Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT),
                Quantities
                  .getQuantity(storageSRated * -1d, PowerSystemUnits.KILOWATT),
                Quantities.getQuantity(storageSRated, PowerSystemUnits.KILOWATT)
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
