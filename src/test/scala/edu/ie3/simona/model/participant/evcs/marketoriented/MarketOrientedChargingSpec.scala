/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.marketoriented

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.model.participant.evcs.SchedulingTimeWindows.SchedulingSliceWithPrice
import edu.ie3.simona.model.participant.evcs.marketoriented.MarketPricePrediction.PredictedPrice
import edu.ie3.simona.test.common.UnitSpec
import tech.units.indriya.quantity.Quantities

class MarketOrientedChargingSpec extends UnitSpec {
  "Determining the relevant scheduling slices" should {
    val getSchedulingSlices = PrivateMethod[Vector[SchedulingSliceWithPrice]](
      Symbol("getSchedulingSlices")
    )

    "provide one slice, if price starts before schedule start and ends before parking ends" in {
      val price = Quantities.getQuantity(5d, StandardUnits.ENERGY_PRICE)
      val predictedPrices = Vector(PredictedPrice(0L, 5L, price))
      val start = 1L
      val end = 10L

      MarketOrientedCharging invokePrivate getSchedulingSlices(
        predictedPrices,
        start,
        end
      ) should contain theSameElementsAs Vector(
        SchedulingSliceWithPrice(1L, 5L, price)
      )
    }

    "provide one slice, if price starts with schedule start and ends before parking ends" in {
      val price = Quantities.getQuantity(5d, StandardUnits.ENERGY_PRICE)
      val predictedPrices = Vector(PredictedPrice(0L, 5L, price))
      val start = 0L
      val end = 10L

      MarketOrientedCharging invokePrivate getSchedulingSlices(
        predictedPrices,
        start,
        end
      ) should contain theSameElementsAs Vector(
        SchedulingSliceWithPrice(0L, 5L, price)
      )
    }

    "provide one slice, if price starts after schedule start and ends after parking ends" in {
      val price = Quantities.getQuantity(5d, StandardUnits.ENERGY_PRICE)
      val predictedPrices = Vector(PredictedPrice(3L, 12L, price))
      val start = 0L
      val end = 10L

      MarketOrientedCharging invokePrivate getSchedulingSlices(
        predictedPrices,
        start,
        end
      ) should contain theSameElementsAs Vector(
        SchedulingSliceWithPrice(3L, 10L, price)
      )
    }

    "provide one slice, if price starts after schedule start and ends when parking ends" in {
      val price = Quantities.getQuantity(5d, StandardUnits.ENERGY_PRICE)
      val predictedPrices = Vector(PredictedPrice(3L, 10L, price))
      val start = 0L
      val end = 10L

      MarketOrientedCharging invokePrivate getSchedulingSlices(
        predictedPrices,
        start,
        end
      ) should contain theSameElementsAs Vector(
        SchedulingSliceWithPrice(3L, 10L, price)
      )
    }

    "provide one slice, if price lays between parking start and end" in {
      val price = Quantities.getQuantity(5d, StandardUnits.ENERGY_PRICE)
      val predictedPrices = Vector(PredictedPrice(3L, 7L, price))
      val start = 0L
      val end = 10L

      MarketOrientedCharging invokePrivate getSchedulingSlices(
        predictedPrices,
        start,
        end
      ) should contain theSameElementsAs Vector(
        SchedulingSliceWithPrice(3L, 7L, price)
      )
    }

    "provide correct slices, if price frames and parking start / end do not match" in {
      val predictedPrices = Vector(
        PredictedPrice(
          0L,
          3L,
          Quantities.getQuantity(5d, StandardUnits.ENERGY_PRICE)
        ),
        PredictedPrice(
          3L,
          7L,
          Quantities.getQuantity(15d, StandardUnits.ENERGY_PRICE)
        ),
        PredictedPrice(
          7L,
          12L,
          Quantities.getQuantity(10d, StandardUnits.ENERGY_PRICE)
        )
      )
      val start = 2L
      val end = 10L

      MarketOrientedCharging invokePrivate getSchedulingSlices(
        predictedPrices,
        start,
        end
      ) should contain theSameElementsAs Vector(
        SchedulingSliceWithPrice(
          2L,
          3L,
          Quantities.getQuantity(5d, StandardUnits.ENERGY_PRICE)
        ),
        SchedulingSliceWithPrice(
          3L,
          7L,
          Quantities.getQuantity(15d, StandardUnits.ENERGY_PRICE)
        ),
        SchedulingSliceWithPrice(
          7L,
          10L,
          Quantities.getQuantity(10d, StandardUnits.ENERGY_PRICE)
        )
      )
    }
  }
}
