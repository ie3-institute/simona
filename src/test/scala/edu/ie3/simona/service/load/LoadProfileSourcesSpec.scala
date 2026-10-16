/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.models.profile.{
  BdewStandardLoadProfile,
  PowerProfileKey,
}
import edu.ie3.simona.config.ConfigParams.BaseCsvParams
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.helper.TestResourceHelper
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import squants.energy.{Kilowatts, Power, Watts}

import scala.jdk.OptionConverters.RichOptional

class LoadProfileSourcesSpec extends UnitSpec with TestResourceHelper {

  private val baseDirectory: String = getResourcePath("_it").toString

  private implicit val powerTolerance: Power = Watts(1e-6)

  "The LoadProfileSources" should {
    val sourceDefinition = Datasource(csvParams =
      Some(BaseCsvParams(",", baseDirectory, isHierarchic = false))
    )

    val markovKey = new PowerProfileKey("test", PowerProfileKey.Type.MARKOV)

    "build sources correctly" in {
      val (profileSources, markovSources) =
        LoadProfileSources.buildSources(sourceDefinition)

      profileSources.size shouldBe 1
      profileSources.contains(BdewStandardLoadProfile.G0.getKey) shouldBe true

      markovSources.size shouldBe 1
      markovSources.contains(markovKey) shouldBe true
    }

    "build Markov sources that provide their model" in {
      val (_, markovSources) =
        LoadProfileSources.buildSources(sourceDefinition)

      val source = markovSources(markovKey)
      source.getProfileKey shouldBe markovKey
      source.getModel.timeModel.samplingIntervalMinutes shouldBe 15
      source.getMaxPower.toScala.map(_.toSquants) match {
        case Some(maxPower) => maxPower should approximate(Kilowatts(4.0))
        case None           => fail("We expect a maximal power here!")
      }
    }

    "skip Markov models that cannot be loaded" in {
      val (profileSources, markovSources) = LoadProfileSources.buildSources(
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

      profileSources shouldBe empty
      markovSources.keySet shouldBe Set(
        new PowerProfileKey("hourly", PowerProfileKey.Type.MARKOV)
      )
    }

    "build no sources without a source definition" in {
      val (profileSources, markovSources) =
        LoadProfileSources.buildSources(Datasource())

      profileSources shouldBe empty
      markovSources shouldBe empty
    }
  }
}
