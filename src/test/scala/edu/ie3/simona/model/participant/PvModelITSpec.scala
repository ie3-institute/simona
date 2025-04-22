/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import org.scalatest.matchers.should.Matchers
import squants.energy.{Megawatts, Power}

/** A simple integration test that uses pre-calculated data to check if the pv
  * model works as expected. It uses 8 pv models located in GER.
  */

class PvModelITSpec extends Matchers with UnitSpec with PvModelITHelper {

  private implicit val tolerance: Power = Megawatts(1e-14)

  "The photovoltaic model" should {
    "match the expected results for all 8 PV panels over the whole year" in {
      val pvModels = createPvModels()
      val weatherMap = getWeatherData
      val resultsMap = getResultsData

      val modelIds = pvModels.keys.toList.sorted
      val keyList = weatherMap.keys.toList.sorted

      keyList.foreach { dateTime =>
        val modelToWeatherMap = weatherMap(dateTime)

        modelIds.map { modelId =>
          val model = pvModels(modelId)
          val weather = modelToWeatherMap(modelId)
          val pvState = PvModel.PvState(
            dateTime.toTick,
            dateTime,
            weather.diffIrr,
            weather.dirIrr,
          )

          val calc = model.determineOperatingPoint(pvState) match {
            case (op, _) => op.activePower
          }

          val sol = resultsMap(dateTime)(modelId)

          calc should approximate(sol)
        }
      }
    }
  }
}
