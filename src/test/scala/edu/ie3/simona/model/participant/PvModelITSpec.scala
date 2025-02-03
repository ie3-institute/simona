/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.test.common.UnitSpec
import org.scalatest.matchers.should.Matchers
import squants.Each
import squants.energy.{Megawatts, Power}

/** A simple integration test that uses pre-calculated data to check if the pv
  * model works as expected. It uses 8 pv models located in GER.
  */

class PvModelITSpec extends Matchers with UnitSpec with PvModelITHelper {

  private implicit val tolerance: Power = Megawatts(
    1e-14
  )

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
          val neededData = PvModel.PvRelevantData(
            dateTime,
            3600L,
            weather.diffIrr,
            weather.dirIrr,
          )

          val voltage = Each(1.414213562d)
          val calc = model
            .calculatePower(0L, voltage, ModelState.ConstantState, neededData)
            .p

          val sol = resultsMap(dateTime)(modelId)

          calc should approximate(sol)
        }
      }
    }
  }
}
