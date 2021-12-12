/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import java.io.InputStreamReader
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

import breeze.numerics.abs
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.BdewLoadProfile._
import edu.ie3.datamodel.models.{BdewLoadProfile, StandardLoadProfile}
import edu.ie3.simona.model.participant.load.profile.{
  LoadProfileKey,
  LoadProfileStore,
  TypeDayProfile
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATTHOUR, WATTHOUR}
import javax.measure.quantity.Energy
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{MINUTE, WATT}

class LoadProfileStoreSpec
    extends UnitSpec
    with PrivateMethodTester
    with TableDrivenPropertyChecks
    with LazyLogging {

  // Load a copy of the standard load profile.
  // This file, in contrast to the working system load
  // profile file, should not be altered.
  val reader = new InputStreamReader(
    this.getClass.getResourceAsStream("/load/standard_load_profiles_test.csv")
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
            55.6d
          ), // Weekday, transitional, 20th quarter hour
          (
            "2019-06-02T00:00:00+02:00[Europe/Berlin]",
            G0,
            68.8d
          ), // Sunday, summer, 0th quarter hour
          (
            "2019-01-05T02:15:00+01:00[Europe/Berlin]",
            H0,
            52.8d
          ), // Saturday, winter, 9th quarter hour, 5th day -> dynamization factor 1.2473
          (
            "2019-07-21T01:00:00+02:00[Europe/Berlin]",
            H0,
            58.1d
          ) // Sunday, summer, 4th quarter hour, 202nd day -> dynamization factor 0.7847
        )

      forAll(params) {
        (
            timestamp: String,
            loadProfile: StandardLoadProfile,
            paramValue: Double
        ) =>
          val time = ZonedDateTime.parse(timestamp)
          val param =
            Quantities.getQuantity(paramValue, WATT)

          customStore.entry(time, loadProfile) shouldBe param
      }
    }

    "return correct max params based on a custom reader" in {
      val maxParams =
        Table(
          ("profile", "maxParamValue"),
          (H0, 268.6d),
          (L0, 240.4d),
          (G0, 240.4d)
        )

      forAll(maxParams) {
        (loadProfile: StandardLoadProfile, maxParamValue: Double) =>
          val maxParam =
            Quantities.getQuantity(maxParamValue, WATT)

          customStore.maxPower(loadProfile) shouldBe maxParam
      }
    }

    "have values, that lead to correct annual energy consumption" in {
      /* Collect all available load profiles */
      val availableLoadProfiles: Set[StandardLoadProfile] =
        (customStore invokePrivate PrivateMethod[
          Map[LoadProfileKey, TypeDayProfile]
        ](Symbol("profileMap"))()).keySet.map(_.standardLoadProfile)

      /* List the expected annual energy consumption */
      val expectedEnergyConsumption
          : Map[StandardLoadProfile, ComparableQuantity[Energy]] = Map(
        BdewLoadProfile.H0 -> Quantities.getQuantity(1000d, KILOWATTHOUR),
        BdewLoadProfile.L0 -> Quantities.getQuantity(1002d, KILOWATTHOUR),
        /* TODO: Check, if this is correct */
        BdewLoadProfile.G0 -> Quantities.getQuantity(1022d, KILOWATTHOUR)
      )

      /* Collect all available time steps in 2020 */
      val startDate =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
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
        val annualEnergy: ComparableQuantity[Energy] = testDates
          .foldLeft(
            Quantities.getQuantity(0d, WATTHOUR)
          )((integrationState, dateTime) => {
            val currentEnergy = customStore
              .entry(dateTime, profile)
              .multiply(Quantities.getQuantity(15, MINUTE))
              .asType(classOf[Energy])
            integrationState.add(currentEnergy)
          })
          .to(KILOWATTHOUR)

        /* Check the deviation against the expected value (deviation should be smaller than 1 kWh) */
        abs(
          annualEnergy
            .subtract(expected)
            .to(KILOWATTHOUR)
            .getValue
            .doubleValue()
        ) < 1 shouldBe true
      })
    }
  }
}
