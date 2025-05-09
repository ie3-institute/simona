/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.config.SimonaConfig.{
  RefSystemConfig,
  VoltLvlConfig,
  VoltageLimitsConfig,
}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.simona.test.common.UnitSpec
import squants.electro.{Kilovolts, Volts}
import squants.energy.{Kilowatts, Megawatts}

class GridConfigParserSpec extends UnitSpec {

  "A GridConfigParser.parseRefSystems" must {
    // check internal gridIdRefSystems
    val gridIdRefSystems =
      PrivateMethod[Map[Int, RefSystem]](Symbol("gridIdRefSystems"))

    // check internal voltLvLRefSystems
    val voltLvLRefSystems =
      PrivateMethod[Map[String, RefSystem]](Symbol("voltLvLRefSystems"))

    "return the default ref systems if no config was provided" in {
      val defaults = GridConfigParser.parseRefSystems(Some(List.empty))

      defaults invokePrivate gridIdRefSystems() shouldBe Map.empty

      defaults invokePrivate voltLvLRefSystems() shouldBe Map(
        GermanVoltageLevelUtils.LV -> RefSystem(Kilowatts(100), Volts(400)),
        GermanVoltageLevelUtils.MV_10KV -> RefSystem(
          Megawatts(40),
          Kilovolts(10),
        ),
        GermanVoltageLevelUtils.MV_20KV -> RefSystem(
          Megawatts(60),
          Kilovolts(20),
        ),
        GermanVoltageLevelUtils.MV_30KV -> RefSystem(
          Megawatts(150),
          Kilovolts(30),
        ),
        GermanVoltageLevelUtils.HV -> RefSystem(Megawatts(600), Kilovolts(110)),
        GermanVoltageLevelUtils.EHV_220KV -> RefSystem(
          Megawatts(800),
          Kilovolts(220),
        ),
        GermanVoltageLevelUtils.EHV_380KV -> RefSystem(
          Megawatts(1000),
          Kilovolts(380),
        ),
      )
    }

    "parse provided valid simona config refSystems correctly" in {
      val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
        Some(
          List(
            RefSystemConfig(
              gridIds = Some(List("1", "2-10", "15...20")),
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            RefSystemConfig(
              gridIds = Some(List("100")),
              sNom = "5000 MVA",
              vNom = "110 kV",
              voltLvls = Some(
                List(
                  VoltLvlConfig("HV", "110 kV"),
                  VoltLvlConfig("EHV", "380 kV"),
                )
              ),
            ),
            RefSystemConfig(
              gridIds = None,
              sNom = "5000 MVA",
              vNom = "110 kV",
              voltLvls = None,
            ),
          )
        )

      val configRefSystems = GridConfigParser.parseRefSystems(validRefSystems)

      // prepare internal value check
      val configRefSystemOne = RefSystem("100 MVA", "10 kV")
      val configRefSystemTwo = RefSystem("5000 MVA", "110 kV")

      configRefSystems invokePrivate gridIdRefSystems() shouldBe Map(
        1 -> configRefSystemOne,
        2 -> configRefSystemOne,
        3 -> configRefSystemOne,
        4 -> configRefSystemOne,
        5 -> configRefSystemOne,
        6 -> configRefSystemOne,
        7 -> configRefSystemOne,
        8 -> configRefSystemOne,
        9 -> configRefSystemOne,
        10 -> configRefSystemOne,
        15 -> configRefSystemOne,
        16 -> configRefSystemOne,
        17 -> configRefSystemOne,
        18 -> configRefSystemOne,
        19 -> configRefSystemOne,
        20 -> configRefSystemOne,
        100 -> configRefSystemTwo,
      )

      configRefSystems invokePrivate voltLvLRefSystems() shouldBe Map(
        GermanVoltageLevelUtils.MV_10KV -> configRefSystemOne,
        GermanVoltageLevelUtils.MV_20KV -> configRefSystemOne,
        GermanVoltageLevelUtils.HV -> configRefSystemTwo,
        GermanVoltageLevelUtils.EHV_380KV -> configRefSystemTwo,
      )

    }

    "throw an InvalidConfigParameterException when provided gridIds contain duplicate entries" in {

      val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
        Some(
          List(
            RefSystemConfig(
              gridIds = Some(List("1", "2", "2", "2-10", "15...20")),
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            )
          )
        )
      intercept[InvalidConfigParameterException] {
        GridConfigParser.parseRefSystems(validRefSystems)
      }.getMessage shouldBe s"The provided gridIds in simona.gridConfig.refSystems contain duplicates. Please check if there are either duplicate entries or overlapping ranges!"

    }

    "throw an InvalidConfigParameterException when provided voltLvls contain duplicate entries" in {

      val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
        Some(
          List(
            RefSystemConfig(
              gridIds = None,
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            RefSystemConfig(
              gridIds = None,
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
          )
        )
      intercept[InvalidConfigParameterException] {
        GridConfigParser.parseRefSystems(validRefSystems)
      }.getMessage shouldBe s"The provided voltLvls in simona.gridConfig.refSystems contain duplicates. Please check your configuration for duplicates in voltLvl entries!"

    }

    "throw an InvalidConfigParameterException when the provided gridId format is unknown" in {

      val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
        Some(
          List(
            RefSystemConfig(
              gridIds = Some(List("asd")),
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            RefSystemConfig(
              gridIds = None,
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
          )
        )
      intercept[InvalidConfigParameterException] {
        GridConfigParser.parseRefSystems(validRefSystems)
      }.getMessage shouldBe "Unknown gridId format asd provided for grid config: RefSystemConfig(Some(List(asd)),100 MVA,10 kV,Some(List(VoltLvlConfig(MV,10 kV), VoltLvlConfig(MV,20 kV))))"

    }

  }

  "A GridConfigParser.parseVoltageLimits" must {
    // check internal gridIdRefSystems
    val gridIdVoltageLimits =
      PrivateMethod[Map[Int, RefSystem]](Symbol("gridIdVoltageLimits"))

    // check internal voltLvLRefSystems
    val voltLvLVoltageLimits =
      PrivateMethod[Map[String, RefSystem]](Symbol("voltLvLVoltageLimits"))

    "return the default voltage limits if no config was provided" in {
      val defaults = GridConfigParser.parseVoltageLimits(Some(List.empty))

      defaults invokePrivate gridIdVoltageLimits() shouldBe Map.empty

      val distributionVoltageLimits = VoltageLimits(0.9, 1.1)

      defaults invokePrivate voltLvLVoltageLimits() shouldBe Map(
        GermanVoltageLevelUtils.LV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.MV_10KV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.MV_20KV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.MV_30KV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.HV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.EHV_220KV -> VoltageLimits(0.9, 1.118),
        GermanVoltageLevelUtils.EHV_380KV -> VoltageLimits(0.9, 1.05),
      )
    }

    "parse provided valid simona config voltage limits correctly" in {
      val validVoltageLimits: Option[List[VoltageLimitsConfig]] =
        Some(
          List(
            VoltageLimitsConfig(
              gridIds = Some(List("1", "2-10", "15...20")),
              vMax = 1.1,
              vMin = 0.9,
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            VoltageLimitsConfig(
              gridIds = Some(List("100")),
              vMax = 1.05,
              vMin = 0.9,
              voltLvls = Some(
                List(
                  VoltLvlConfig("HV", "110 kV"),
                  VoltLvlConfig("EHV", "380 kV"),
                )
              ),
            ),
            VoltageLimitsConfig(
              gridIds = None,
              vMax = 1.1,
              vMin = 0.9,
              voltLvls = None,
            ),
          )
        )

      val configVoltageLimits =
        GridConfigParser.parseVoltageLimits(validVoltageLimits)

      // prepare internal value check
      val configVoltageLimitsOne = VoltageLimits(0.9, 1.1)
      val configVoltageLimitsTwo = VoltageLimits(0.9, 1.05)

      configVoltageLimits invokePrivate gridIdVoltageLimits() shouldBe Map(
        1 -> configVoltageLimitsOne,
        2 -> configVoltageLimitsOne,
        3 -> configVoltageLimitsOne,
        4 -> configVoltageLimitsOne,
        5 -> configVoltageLimitsOne,
        6 -> configVoltageLimitsOne,
        7 -> configVoltageLimitsOne,
        8 -> configVoltageLimitsOne,
        9 -> configVoltageLimitsOne,
        10 -> configVoltageLimitsOne,
        15 -> configVoltageLimitsOne,
        16 -> configVoltageLimitsOne,
        17 -> configVoltageLimitsOne,
        18 -> configVoltageLimitsOne,
        19 -> configVoltageLimitsOne,
        20 -> configVoltageLimitsOne,
        100 -> configVoltageLimitsTwo,
      )

      configVoltageLimits invokePrivate voltLvLVoltageLimits() shouldBe Map(
        GermanVoltageLevelUtils.MV_10KV -> configVoltageLimitsOne,
        GermanVoltageLevelUtils.MV_20KV -> configVoltageLimitsOne,
        GermanVoltageLevelUtils.HV -> configVoltageLimitsTwo,
        GermanVoltageLevelUtils.EHV_380KV -> configVoltageLimitsTwo,
      )

    }

    "throw an InvalidConfigParameterException when provided gridIds contain duplicate entries" in {

      val validVoltageLimits: Option[List[VoltageLimitsConfig]] =
        Some(
          List(
            VoltageLimitsConfig(
              gridIds = Some(List("1", "2", "2", "2-10", "15...20")),
              vMax = 1.1,
              vMin = 0.9,
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            )
          )
        )
      intercept[InvalidConfigParameterException] {
        GridConfigParser.parseVoltageLimits(validVoltageLimits)
      }.getMessage shouldBe s"The provided gridIds in simona.gridConfig.voltageLimits contain duplicates. Please check if there are either duplicate entries or overlapping ranges!"

    }

    "throw an InvalidConfigParameterException when provided voltLvls contain duplicate entries" in {

      val validVoltageLimits: Option[List[VoltageLimitsConfig]] =
        Some(
          List(
            VoltageLimitsConfig(
              gridIds = None,
              vMax = 1.1,
              vMin = 0.9,
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            VoltageLimitsConfig(
              gridIds = None,
              vMax = 1.1,
              vMin = 0.9,
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
          )
        )
      intercept[InvalidConfigParameterException] {
        GridConfigParser.parseVoltageLimits(validVoltageLimits)
      }.getMessage shouldBe s"The provided voltLvls in simona.gridConfig.voltageLimits contain duplicates. Please check your configuration for duplicates in voltLvl entries!"

    }

    "throw an InvalidConfigParameterException when the provided gridId format is unknown" in {

      val validVoltageLimits: Option[List[VoltageLimitsConfig]] =
        Some(
          List(
            VoltageLimitsConfig(
              gridIds = Some(List("asd")),
              vMax = 1.1,
              vMin = 0.9,
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            VoltageLimitsConfig(
              gridIds = None,
              vMax = 1.1,
              vMin = 0.9,
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
          )
        )
      intercept[InvalidConfigParameterException] {
        GridConfigParser.parseVoltageLimits(validVoltageLimits)
      }.getMessage shouldBe "Unknown gridId format asd provided for grid config: VoltageLimitsConfig(Some(List(asd)),1.1,0.9,Some(List(VoltLvlConfig(MV,10 kV), VoltLvlConfig(MV,20 kV))))"

    }

  }

  "A valid ConfigRefSystem" must {

    val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
      Some(
        List(
          RefSystemConfig(
            gridIds = Some(List("1", "2-10", "15...20")),
            sNom = "100 MVA",
            vNom = "10 kV",
            voltLvls = Some(
              List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
            ),
          ),
          RefSystemConfig(
            gridIds = Some(List("100")),
            sNom = "5000 MVA",
            vNom = "110 kV",
            voltLvls = Some(
              List(
                VoltLvlConfig("HV", "110 kV"),
                VoltLvlConfig("EHV", "380 kV"),
              )
            ),
          ),
        )
      )

    val configRefSystems = GridConfigParser.parseRefSystems(validRefSystems)

    // prepare expected RefSystems
    val configRefSystemOne = RefSystem("100 MVA", "10 kV")
    val configRefSystemTwo = RefSystem("5000 MVA", "110 kV")

    "find the corresponding RefSystem for a present gridId if no voltLvl is provided" in {

      configRefSystems.find(1) shouldBe Some(configRefSystemOne)

    }

    "find the corresponding RefSystem for a present gridId if a voltLvl is provided" in {

      configRefSystems.find(
        1,
        Some(GermanVoltageLevelUtils.MV_10KV),
      ) shouldBe Some(
        configRefSystemOne
      )

    }

    "find the corresponding RefSystem for a non-present gridId if a voltLvl is provided" in {

      configRefSystems.find(
        1000,
        Some(GermanVoltageLevelUtils.HV),
      ) shouldBe Some(
        configRefSystemTwo
      )

    }

    "return None if a RefSystem cannot be found by it's gridId and no voltLvl is provided" in {

      configRefSystems.find(1000) shouldBe None

    }

    "return None if a RefSystem cannot be found neither by its gridId nor by its voltLvl" in {

      configRefSystems.find(
        1000,
        Some(GermanVoltageLevelUtils.EHV_220KV),
      ) shouldBe None

    }

  }

}
