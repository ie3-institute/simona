/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.simona.test.common.UnitSpec
import org.scalatest.matchers.should.Matchers
import squants.{Dimensionless, Each}
import squants.energy.Power

import java.time.ZonedDateTime
import java.util.Locale
import scala.collection.mutable.ArrayBuffer

/** A simple integration test that uses pre-calculated data to check if the pv
  * model works as expected. It uses 8 pv models located in GER.
  */

class PvModelIT extends UnitSpec with Matchers with PvModelITHelper {

  Locale.setDefault(Locale.US)
  val pvModels: Map[String, PvModel] = getCreatePvModels
  val weatherMap: Map[ZonedDateTime, Map[String, WeatherMessage.WeatherData]] =
    getWeatherData
  val resultsMap: Map[ZonedDateTime, Map[String, Power]] = getResultsData

  "8 pv panels full year test" should {
    "yield results close to the expected data" in {
      val testRes = ArrayBuffer[Double]()

      val modelIds = pvModels.keys.toList.sorted
      val keyList = weatherMap.keys.toList.sorted

      keyList.foreach { dateTime =>
        val modelToWeatherMap = weatherMap(dateTime)

        var modelI = 0
        modelIds.foreach { modelId =>
          val model = pvModels(modelId)
          val weather = modelToWeatherMap(modelId)

          val neededData = PvModel.PvRelevantData(
            dateTime,
            3600L,
            weather.diffIrr,
            weather.dirIrr,
          )
          val voltage: Dimensionless = Each(1.414213562d)

          val calc = model
            .calculatePower(0L, voltage, ModelState.ConstantState, neededData)
            .p
            .toMegawatts
          val sol = resultsMap(dateTime)(modelId).toMegawatts

          testRes += Math.abs(calc - sol)

          modelI += 1
        }
      }
      testRes.forall(_ < 1e-14) shouldBe true
    }
  }
}
