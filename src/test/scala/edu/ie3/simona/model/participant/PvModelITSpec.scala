/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.simona.util.TickUtil.toTick
import squants.energy.{Megawatts, Power}
import java.time.temporal.ChronoUnit

/** A simple integration test that uses pre-calculated data to check if the pv
  * model works as expected. It uses 8 pv models located in GER.
  */
class PvModelITSpec extends UnitSpec with PvModelITHelper with DefaultTestData {

  private implicit val tolerance: Power = Megawatts(1e-14)

  "The photovoltaic model" should {
    "match the expected results for all 8 PV panels over the whole year" in {
      val pvModels = createPvModels()
      val weatherMap = getWeatherData
      val resultsMap = getResultsData

      val modelIds = pvModels.keys.toList.sorted
      val keyList = weatherMap.keys.toList.sorted

      keyList shouldNot be(empty)

      val dataResolutionSeconds =
        ChronoUnit.SECONDS.between(keyList.head, keyList(1))
      val expectedStart = keyList.head
      val expectedEnd = keyList.last
      expectedStart.getYear shouldBe 2011

      val totalDurationsSeconds =
        ChronoUnit.SECONDS.between(expectedStart, expectedEnd)
      val expectedSteps = (totalDurationsSeconds / dataResolutionSeconds + 1)
      keyList.size.toLong shouldBe expectedSteps

      resultsMap.keys.toList.sorted shouldBe keyList

      keyList.foreach { dateTime =>
        val modelToWeatherMap = weatherMap(dateTime)

        modelIds.map { modelId =>
          val model = pvModels(modelId)
          val weather = modelToWeatherMap(modelId)
          val pvState = PvModel.PvState(
            tick = dateTime.toTick,
            dateTime = dateTime,
            dirIrradiance = weather.dirIrr,
            diffIrradiance = weather.diffIrr,
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
