/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load.profile

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile._
import edu.ie3.datamodel.models.profile.StandardLoadProfile
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Watts}
import squants.time.Minutes

import java.io.InputStreamReader
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

class LoadProfileStoreSpec
    extends UnitSpec
    with PrivateMethodTester
    with TableDrivenPropertyChecks
    with LazyLogging {

  // Load a copy of the standard load profile.
  // This file, in contrast to the working system load
  // profile file, should not be altered.
  val reader = new InputStreamReader(
    this.getClass.getResourceAsStream("standard_load_profiles_test.csv")
  )

  val customStore: LoadProfileStore = LoadProfileStore(reader)

  "A LoadProfileStore" must {
    "return the correct values based on a custom reader" in {
      val params =
        Table(
          ("timestamp", "consumerType", "param"),
          (
            "2019-04-01T05:00:00+02:00[Europe/Berlin]",
            L0,
            55.6d,
          ), // Weekday, transitional, 20th quarter-hour
          (
            "2019-06-02T00:00:00+02:00[Europe/Berlin]",
            G0,
            68.8d,
          ), // Sunday, summer, 0th quarter-hour
          (
            "2019-01-05T02:15:00+01:00[Europe/Berlin]",
            H0,
            52.8d,
          ), // Saturday, winter, 9th quarter-hour, 5th day -> dynamization factor 1.2473
          (
            "2019-07-21T01:00:00+02:00[Europe/Berlin]",
            H0,
            58.1d,
          ), // Sunday, summer, 4th quarter-hour, 202nd day -> dynamization factor 0.7847
        )

      forAll(params) {
        (
            timestamp: String,
            loadProfile: StandardLoadProfile,
            paramValue: Double,
        ) =>
          val time = ZonedDateTime.parse(timestamp)
          val param = Watts(paramValue)

          customStore.entry(time, loadProfile) shouldBe param
      }
    }

    "return correct max params based on a custom reader" in {
      val maxParams =
        Table(
          ("profile", "maxParamValue"),
          (H0, 268.6d),
          (L0, 240.4d),
          (G0, 240.4d),
        )

      forAll(maxParams) {
        (loadProfile: StandardLoadProfile, maxParamValue: Double) =>
          val maxParam = Watts(maxParamValue)

          customStore.maxPower(loadProfile) shouldBe maxParam
      }
    }

    "have values, that lead to correct annual energy consumption" in {
      /* Collect all available load profiles */
      val availableLoadProfiles: Set[StandardLoadProfile] =
        customStore.loadProfiles

      /* List the expected annual energy consumption */
      val expectedEnergyConsumption: Map[StandardLoadProfile, squants.Energy] =
        Map(
          H0 -> KilowattHours(1000.0),
          L0 -> KilowattHours(1002.0),
          /* TODO: Check, if this is correct */
          G0 -> KilowattHours(1022.0),
        )

      /* Collect all available time steps in 2020 */
      val startDate =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")
      val testDates =
        Range(0, 35136).map(cnt => startDate.plus(cnt * 15, ChronoUnit.MINUTES))

      /* Check every load profile */
      availableLoadProfiles.foreach(profile => {
        /* Get expected value */
        val expected = expectedEnergyConsumption.get(profile) match {
          case Some(expectedValue) => expectedValue
          case None =>
            fail(
              s"There is no expected annual energy consumption for '$profile'"
            )
        }

        /* Sum up value throughout the year (2020 is a leap year) */
        val annualEnergy: squants.Energy = testDates
          .foldLeft(
            KilowattHours(0.0)
          )((integrationState, dateTime) => {
            val currentEnergy = customStore
              .entry(dateTime, profile) * Minutes(15.0)
            integrationState + currentEnergy
          })

        implicit val powerTolerance: squants.Energy = KilowattHours(1.0)

        /* Check the deviation against the expected value (deviation should be smaller than 1 kWh) */
        annualEnergy should approximate(expected)
      })
    }
  }
}
