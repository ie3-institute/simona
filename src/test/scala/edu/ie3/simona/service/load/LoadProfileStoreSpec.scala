/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.models.profile.LoadProfile.{
  DefaultLoadProfiles,
  RandomLoadProfile,
}
import edu.ie3.datamodel.models.profile.{BdewStandardLoadProfile, LoadProfile}
import edu.ie3.simona.test.common.UnitSpec
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar.mock

import java.util.Optional
import scala.util.Try

class LoadProfileStoreSpec extends UnitSpec {

  private val loadProfile1 = new LoadProfile {
    override def getKey: String = "lp1"
  }

  private val loadProfile2 = new LoadProfile {
    override def getKey: String = "lp2"
  }

  "The load profile store" should {

    "be create correctly" in {
      val buildInsStore = LoadProfileStore()

      BdewStandardLoadProfile
        .values()
        .foreach(profile =>
          Try(buildInsStore.valueProvider(profile)).isSuccess shouldBe true
        )

      Try(
        buildInsStore.valueProvider(RandomLoadProfile.RANDOM_LOAD_PROFILE)
      ).isSuccess shouldBe true

      Try(
        buildInsStore.valueProvider(DefaultLoadProfiles.NO_LOAD_PROFILE)
      ).isFailure shouldBe true

    }

    "return known load profiles correctly" in {
      val buildInsStore = LoadProfileStore()

      val buildInsProfiles = BdewStandardLoadProfile.values().toSet ++ Set(
        LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE
      )

      val store = LoadProfileStore(
        Map(
          loadProfile1 -> mockSource,
          loadProfile2 -> mockSource,
        )
      )

      buildInsStore.getKnownProfiles shouldBe buildInsProfiles
      store.getKnownProfiles shouldBe buildInsProfiles ++ Set(
        loadProfile1,
        loadProfile2,
      )
    }

  }

  private def mockSource: LoadProfileSource[_, _] = {
    val source = mock[LoadProfileSource[_, _]]
    when(source.getMaxPower).thenReturn(Optional.empty())
    source
  }
}
