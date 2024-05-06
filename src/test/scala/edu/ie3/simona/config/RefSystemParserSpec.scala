/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.config.SimonaConfig.{RefSystemConfig, VoltLvlConfig}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.test.common.UnitSpec
import squants.electro.{Kilovolts, Volts}
import squants.energy.{Kilowatts, Megawatts}

class RefSystemParserSpec extends UnitSpec {

  "A RefSystemParser" must {
    // check internal gridIdRefSystems
    val gridIdRefSystems =
      PrivateMethod[Map[Int, RefSystem]](Symbol("gridIdRefSystems"))

    // check internal voltLvLRefSystems
    val voltLvLRefSystems =
      PrivateMethod[Map[String, RefSystem]](Symbol("voltLvLRefSystems"))

    "return the default ref systems if no config was provided" in {
      val defaults = RefSystemParser.parse(Some(List.empty))

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
            new RefSystemConfig(
              gridIds = Some(List("1", "2-10", "15...20")),
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            new RefSystemConfig(
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
            new RefSystemConfig(
              gridIds = None,
              sNom = "5000 MVA",
              vNom = "110 kV",
              voltLvls = None,
            ),
          )
        )

      val configRefSystems = RefSystemParser.parse(validRefSystems)

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
            new RefSystemConfig(
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
        RefSystemParser.parse(validRefSystems)
      }.getMessage shouldBe s"The provided gridIds in simona.gridConfig.refSystems contain duplicates. Please check if there are either duplicate entries or overlapping ranges!"

    }

    "throw an InvalidConfigParameterException when provided voltLvls contain duplicate entries" in {

      val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
        Some(
          List(
            new RefSystemConfig(
              gridIds = None,
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            new RefSystemConfig(
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
        RefSystemParser.parse(validRefSystems)
      }.getMessage shouldBe s"The provided voltLvls in simona.gridConfig.refSystems contain duplicates. Please check your configuration for duplicates in voltLvl entries!"

    }

    "throw an InvalidConfigParameterException when the provided gridId format is unknown" in {

      val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
        Some(
          List(
            new RefSystemConfig(
              gridIds = Some(List("asd")),
              sNom = "100 MVA",
              vNom = "10 kV",
              voltLvls = Some(
                List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
              ),
            ),
            new RefSystemConfig(
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
        RefSystemParser.parse(validRefSystems)
      }.getMessage shouldBe "Unknown gridId format asd provided for refSystem RefSystemConfig(Some(List(asd)),100 MVA,10 kV,Some(List(VoltLvlConfig(MV,10 kV), VoltLvlConfig(MV,20 kV))))"

    }

  }

  "A valid ConfigRefSystem" must {

    val validRefSystems: Option[List[SimonaConfig.RefSystemConfig]] =
      Some(
        List(
          new RefSystemConfig(
            gridIds = Some(List("1", "2-10", "15...20")),
            sNom = "100 MVA",
            vNom = "10 kV",
            voltLvls = Some(
              List(VoltLvlConfig("MV", "10 kV"), VoltLvlConfig("MV", "20 kV"))
            ),
          ),
          new RefSystemConfig(
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

    val configRefSystems = RefSystemParser.parse(validRefSystems)

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
