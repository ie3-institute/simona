/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.source.PowerValueSource.MarkovIdentifier
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE
import edu.ie3.datamodel.models.profile.{
  BdewStandardLoadProfile,
  LoadProfile,
  PowerProfileKey,
}
import edu.ie3.simona.config.ConfigParams.BaseCsvParams
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.load.MarkovLoadModel.MarkovLoadFactoryData
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.helper.TestResourceHelper
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import squants.energy.*

import java.time.ZonedDateTime
import java.util.{OptionalDouble, OptionalInt}

class LoadProfileStoreSpec extends UnitSpec with TestResourceHelper {

  private val store: LoadProfileStore = LoadProfileStore()

  private val markovStore: LoadProfileStore = LoadProfileStore(
    Datasource(csvParams =
      Some(
        BaseCsvParams(
          ",",
          getResourcePath("_markov").toString,
          isHierarchic = false,
        )
      )
    )
  )

  // Markov model with a sampling interval of 60 minutes
  private val markovKey =
    new PowerProfileKey("hourly", PowerProfileKey.Type.MARKOV)

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

    "contain Markov profiles of a source definition" in {
      markovStore.profileToMarkovSource.keySet shouldBe Set(markovKey)
      markovStore.contains(markovKey) shouldBe true

      // same name, but time series profile
      markovStore.contains(new PowerProfileKey("hourly")) shouldBe false
    }

    "return the profile resolutions including Markov profiles" in {
      val resolutions = markovStore.getProfileResolutions

      resolutions(BdewStandardLoadProfile.G0.getKey) shouldBe 900L
      resolutions(markovKey) shouldBe 3600L
    }

    "find the next activation tick of Markov profiles" in {
      given ZonedDateTime =
        TimeUtil.withDefaults.toZonedDateTime("2024-01-03T00:00:00Z")

      val onlyMarkovStore = LoadProfileStore(
        Map.empty,
        markovStore.profileToMarkovSource,
      )

      onlyMarkovStore.getNextActivationTick(0L) shouldBe Some(3600L)
      onlyMarkovStore.getNextActivationTick(3600L) shouldBe Some(7200L)
    }

    "return a step function for a Markov load profile" in {
      val source = markovStore.profileToMarkovSource(markovKey)

      val cases = Table(
        ("requestedTime", "previousState", "seed"),
        (time, 0, 42L),
        (time, 1, 42L),
        (time, 1, 7L),
        (time.plusHours(5), 1, 7L),
      )

      forAll(cases) { (requestedTime, previousState, seed) =>
        val expected = source
          .getValueSupplier(
            new MarkovIdentifier(
              requestedTime,
              OptionalInt.of(previousState),
              OptionalDouble.empty(),
              seed,
            )
          )
          .get

        val (power, nextState) =
          markovStore.markovEntryFunc(requestedTime, markovKey)(
            previousState,
            seed,
          )

        power shouldBe expected.value.get.getP.get.toSquants
        nextState shouldBe expected.nextState
      }
    }

    "throw an exception for a Markov load profile that is not available" in {
      intercept[CriticalFailureException] {
        markovStore.markovEntryFunc(
          time,
          new PowerProfileKey("other", PowerProfileKey.Type.MARKOV),
        )
      }
    }

    "return Markov load factory data correctly" in {
      markovStore.getMarkovLoadFactoryData(markovKey) match {
        case Some(
              MarkovLoadFactoryData(
                maxPower,
                energyScaling,
                resolution,
                stepFunc,
              )
            ) =>
          maxPower match {
            case Some(power) => power should approximate(Kilowatts(4d))
            case None        => fail("We expect a maximal power here!")
          }
          energyScaling shouldBe None
          resolution shouldBe 3600L

          Seq(time, time.plusHours(5)).foreach { requestedTime =>
            stepFunc(requestedTime, 1, 7L) shouldBe markovStore.markovEntryFunc(
              requestedTime,
              markovKey,
            )(1, 7L)
          }

        case None =>
          fail("We expect factory data here!")
      }

      // no factory data of the other profile type
      markovStore.getProfileLoadFactoryData(markovKey) shouldBe None
      markovStore.getMarkovLoadFactoryData(
        BdewStandardLoadProfile.G0.getKey
      ) shouldBe None
    }
  }
}
