/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.simona.config.ConfigParams.BaseCsvParams
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.helper.TestResourceHelper

class LoadProfileSourcesSpec extends UnitSpec with TestResourceHelper {

  private val baseDirectory: String = getResourcePath("_it").toString

  "The LoadProfileSources" should {
    val sourceDefinition = Datasource(csvParams =
      Some(BaseCsvParams(",", baseDirectory, isHierarchic = false))
    )

    "build sources correctly" in {
      val profileSources = LoadProfileSources.buildSources(sourceDefinition)

      profileSources.size shouldBe 1
      profileSources.contains(BdewStandardLoadProfile.G0.getKey) shouldBe true
    }

  }
}
