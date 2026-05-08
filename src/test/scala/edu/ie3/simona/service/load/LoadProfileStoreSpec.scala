/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE
import edu.ie3.datamodel.models.profile.{
  BdewStandardLoadProfile,
  LoadProfile,
  PowerProfileKey,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import squants.energy.*

class LoadProfileStoreSpec extends UnitSpec {

  private val store: LoadProfileStore = LoadProfileStore()

  private implicit val powerTolerance: Power = Watts(1e-6)
  private implicit val energyTolerance: Energy = WattHours(1e-6)

  "A LoadProfileStore" should {

    val time =
      TimeUtil.withDefaults.toZonedDateTime("2024-01-03T00:00:00Z") // wednesday

    "contain all build-in profile sources" in {
      val profiles = store.profileToSource.keySet

      val buildInSources =
        (BdewStandardLoadProfile.values().toSet ++ Set(RANDOM_LOAD_PROFILE))
          .map(_.getKey)

      profiles should contain allElementsOf buildInSources
    }

    "be able to check, if it contains a given load profile" in {
      val otherProfile = new LoadProfile {
        override def getKey: PowerProfileKey = new PowerProfileKey("other")
      }

      val cases = Table(
        ("loadProfile", "expectedResult"),
        (BdewStandardLoadProfile.H0, true),
        (RANDOM_LOAD_PROFILE, true),
        (otherProfile, false),
      )

      forAll(cases) { (loadProfile, expectedResult) =>
        store.contains(loadProfile.getKey) shouldBe expectedResult
      }
    }

    "return a value for a given time and load profile" in {
      val func = store.entryFunc(time, BdewStandardLoadProfile.G0.getKey)
      func() should approximate(Watts(65.5))
    }

    "sample multiple random values for random load profile" in {
      val func = store.entryFunc(time, RANDOM_LOAD_PROFILE.getKey)
      val powers = Range(0, 10).map(_ => func())

      powers.size shouldBe 10
      powers.toSet.size > 1 shouldBe true
    }

    "return profile load factory data correctly" in {
      val cases = Table[LoadProfile, Power, Energy](
        ("loadProfile", "expectedMaxPower", "expectedProfileScaling"),
        (BdewStandardLoadProfile.G0, Watts(240.4), KilowattHours(1000)),
        (RANDOM_LOAD_PROFILE, Watts(159), KilowattHours(716.5416966513656)),
      )

      forAll(cases) { (loadProfile, expectedMaxPower, expectedProfileScaling) =>
        val factoryData = store.getProfileLoadFactoryData(loadProfile.getKey)

        factoryData.flatMap { data =>
          data.maxPower.zip(data.energyScaling)
        } match {
          case Some((maxPower, energyScaling)) =>
            maxPower should approximate(expectedMaxPower)
            energyScaling should approximate(expectedProfileScaling)

          case _ if store.contains(loadProfile.getKey) =>
            fail("We expect factory data here!")

          case _ =>
            fail("This should not happen!")
        }
      }
    }
  }
}
