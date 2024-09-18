/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.profile.LoadProfile.{
  DefaultLoadProfiles,
  RandomLoadProfile,
}
import edu.ie3.simona.test.common.UnitSpec

import scala.jdk.CollectionConverters._
import scala.util.Try

class LoadProfileStoreSpec extends UnitSpec {

  "The load profile store" should {
    "be create correctly" in {
      val buildInsStore = LoadProfileStore(
        Some(LoadProfileSource.getBDEWLoadProfiles.asScala.toMap),
        Some(LoadProfileSource.getRandomLoadProfile),
        Map.empty,
      )

      BdewStandardLoadProfile
        .values()
        .foreach(profile =>
          Try(buildInsStore.getValueProvider(profile)).isSuccess shouldBe true
        )

      Try(
        buildInsStore.getValueProvider(RandomLoadProfile.RANDOM_LOAD_PROFILE)
      ).isSuccess shouldBe true

      Try(
        buildInsStore.getValueProvider(DefaultLoadProfiles.NO_LOAD_PROFILE)
      ).isFailure shouldBe true

    }
  }

}
