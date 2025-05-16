/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
  Transformer3WResult,
}
import edu.ie3.datamodel.models.result.system.LoadResult
import edu.ie3.datamodel.models.result.{
  CongestionResult,
  NodeResult,
  ResultEntity,
}
import edu.ie3.simona.config.ConfigParams.ResultKafkaParams
import edu.ie3.simona.config.OutputConfig.GridOutputConfig
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.config.SimonaConfig.{apply as _, *}
import edu.ie3.simona.config.{OutputConfig, SimonaConfig}
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.ConfigUtil.NotifierIdentifier.*
import edu.ie3.simona.util.ConfigUtil.*
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

import java.util.UUID

class ConfigUtilSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with ConfigTestData {

  "The em config util" should {
    "be created correctly with valid data" in {
      val emRuntimeConfig = ConfigFactory.parseString(
        """simona.runtime.em = {
          |  defaultConfig = {
          |    uuids = ["default"]
          |    scaling = 1.0
          |    }
          |  individualConfigs = []
          |}""".stripMargin
      )
      val config =
        emRuntimeConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.EmConfigUtil(
        simonaConfig.simona.runtime.em
      )

      inside(actual) { case EmConfigUtil(configs, defaultConfigs) =>
        configs shouldBe Map.empty[UUID, EmRuntimeConfig]

        defaultConfigs.calculateMissingReactivePowerWithModel shouldBe false
        defaultConfigs.scaling shouldBe 1.0
        defaultConfigs.uuids shouldBe List("default")
        defaultConfigs.aggregateFlex shouldBe "SELF_OPT_EXCL_REG"
        defaultConfigs.curtailRegenerative shouldBe false
      }
    }

    "be created correctly with one UUID correctly" in {
      val emRuntimeConfig = ConfigFactory.parseString(
        """simona.runtime.em = {
          |  defaultConfig = {
          |    uuids = ["default"]
          |    scaling = 1.0
          |    }
          |  individualConfigs = [
          |    {
          |      calculateMissingReactivePowerWithModel = false
          |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
          |      scaling = 1.3
          |      curtailRegenerative = true
          |    }
          |  ]
          |}""".stripMargin
      )
      val config =
        emRuntimeConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.EmConfigUtil(
        simonaConfig.simona.runtime.em
      )

      inside(actual) { case EmConfigUtil(configs, _) =>
        configs.size shouldBe 1

        val individual =
          configs(UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76"))

        individual.calculateMissingReactivePowerWithModel shouldBe false
        individual.scaling shouldBe 1.3
        individual.uuids shouldBe List("49f250fa-41ff-4434-a083-79c98d260a76")
        individual.aggregateFlex shouldBe "SELF_OPT_EXCL_REG"
        individual.curtailRegenerative shouldBe true
      }
    }
  }

  "The participant config util" should {
    "setup correctly with valid load data" in {
      val loadRuntimeConfig = ConfigFactory.parseString(
        """simona.runtime.participant.load = {
          |  defaultConfig = {
          |    uuids = ["default"]
          |    scaling = 1.0
          |    modelBehaviour = "fix"
          |    reference = "power"
          |    }
          |  individualConfigs = []
          |}""".stripMargin
      )
      val config =
        loadRuntimeConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs shouldBe Map.empty[UUID, LoadRuntimeConfig]
        defaultConfigs.size shouldBe 8

        inside(defaultConfigs.get(classOf[LoadRuntimeConfig])) {
          case Some(
                LoadRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids,
                  modelBehaviour,
                  reference,
                )
              ) =>
            calculateMissingReactivePowerWithModel shouldBe false
            modelBehaviour shouldBe "fix"
            reference shouldBe "power"
            scaling shouldBe 1.0
            uuids shouldBe List("default")
          case unexpected =>
            fail(
              s"Expected a default $LoadRuntimeConfig. I got '$unexpected"
            )
        }
      }
    }

    "setup on one valid load input config with one UUID correctly" in {
      val loadRuntimeConfig = ConfigFactory.parseString(
        """simona.runtime.participant.load = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.3
          |    modelBehaviour = "profile"
          |    reference = "power"
          |    }
          |  individualConfigs = [
          |    {
          |      calculateMissingReactivePowerWithModel = false
          |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
          |      scaling = 1.3
          |      modelBehaviour = "profile"
          |      reference = "power"
          |    }
          |  ]
          |}""".stripMargin
      )
      val config =
        loadRuntimeConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs.size shouldBe 1
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )

        defaultConfigs.size shouldBe 8
        inside(defaultConfigs.get(classOf[LoadRuntimeConfig])) {
          case Some(
                LoadRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids,
                  modelBehaviour,
                  reference,
                )
              ) =>
            calculateMissingReactivePowerWithModel shouldBe false
            modelBehaviour shouldBe "profile"
            reference shouldBe "power"
            scaling shouldBe 1.3
            uuids shouldBe List("default")
          case unexpected =>
            fail(
              s"Expected a default $LoadRuntimeConfig. I got '$unexpected"
            )
        }
      }
    }

    "setup on one valid load input config with multiple UUIDs correctly" in {
      val loadRuntimeConfig = ConfigFactory.parseString(
        """simona.runtime.participant.load = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    baseModelConfig.uuids = ["default"]
          |    baseModelConfig.scaling = 1.3
          |    modelBehaviour = "profile"
          |    reference = "power"
          |    }
          |  individualConfigs = [
          |    {
          |      calculateMissingReactivePowerWithModel = false
          |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76", "fb8f1443-1843-4ecd-a94a-59be8148397f"]
          |      scaling = 1.3
          |      modelBehaviour = "profile"
          |      reference = "power"
          |    }
          |  ]
          |}""".stripMargin
      )
      val config =
        loadRuntimeConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, _) =>
        configs.size shouldBe 2
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )
        configs.contains(
          UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
        )
      }

      actual.getOrDefault[LoadRuntimeConfig](
        UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe actual.getOrDefault[LoadRuntimeConfig](
        UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
      )
    }

    "setup on multiple correct load input configs correctly" in {
      val loadRuntimeConfig = ConfigFactory.parseString(
        """simona.runtime.participant.load = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.3
          |    modelBehaviour = "profile"
          |    reference = "power"
          |    }
          |  individualConfigs = [
          |    {
          |     calculateMissingReactivePowerWithModel = false
          |     uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
          |     scaling = 1.3
          |     modelBehaviour = "profile"
          |     reference = "power"
          |  },
          |  {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["fb8f1443-1843-4ecd-a94a-59be8148397f"]
          |    scaling = 1.5
          |    modelBehaviour = "random"
          |    reference = "energy"
          |  }
          |  ]
          |}""".stripMargin
      )
      val config =
        loadRuntimeConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, _) =>
        configs.size shouldBe 2
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )
        configs.contains(
          UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
        )
      }

      actual.getOrDefault[LoadRuntimeConfig](
        UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        LoadRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          1.3,
          List("49f250fa-41ff-4434-a083-79c98d260a76"),
          "profile",
        )
      actual.getOrDefault[LoadRuntimeConfig](
        UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
      ) shouldBe
        LoadRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          1.5,
          List("fb8f1443-1843-4ecd-a94a-59be8148397f"),
          "random",
          "energy",
        )
    }
  }

  "The participant config util" should {
    "setup on correct fixed feed in input data correctly" in {
      val fixedFeedInModelConfig = ConfigFactory.parseString(
        """simona.runtime.participant.fixedFeedIn = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.0
          |    }
          |  individualConfigs = []
          |}""".stripMargin
      )
      val config =
        fixedFeedInModelConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs shouldBe Map.empty[UUID, FixedFeedInRuntimeConfig]

        inside(defaultConfigs.get(classOf[FixedFeedInRuntimeConfig])) {
          case Some(
                FixedFeedInRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids,
                )
              ) =>
            calculateMissingReactivePowerWithModel shouldBe false
            scaling shouldBe 1.0
            uuids shouldBe List("default")
          case unexpected =>
            fail(
              s"Expected a default $FixedFeedInRuntimeConfig. I got '$unexpected"
            )
        }
      }
    }

    "setup on one correct fixed feed in input config with one UUID correctly" in {
      val fixedFeedInModelConfig = ConfigFactory.parseString(
        """simona.runtime.participant.fixedFeedIn = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.3
          |    }
          |  individualConfigs = [
          |    {
          |      calculateMissingReactivePowerWithModel = false
          |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
          |      scaling = 1.3
          |    }
          |  ]
          |}""".stripMargin
      )
      val config =
        fixedFeedInModelConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs.size shouldBe 1
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )

        defaultConfigs.size shouldBe 8
        inside(defaultConfigs.get(classOf[FixedFeedInRuntimeConfig])) {
          case Some(
                FixedFeedInRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids,
                )
              ) =>
            calculateMissingReactivePowerWithModel shouldBe false
            scaling shouldBe 1.3
            uuids shouldBe List("default")
          case unexpected =>
            fail(
              s"Expected a default $FixedFeedInRuntimeConfig. I got '$unexpected"
            )
        }
      }
    }

    "setup on one correct fixed feed in input config with multiple UUIDs correctly" in {
      val fixedFeedInModelConfig = ConfigFactory.parseString(
        """simona.runtime.participant.fixedFeedIn = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.3
          |    }
          |  individualConfigs = [
          |    {
          |      calculateMissingReactivePowerWithModel = false
          |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76", "fb8f1443-1843-4ecd-a94a-59be8148397f"]
          |      scaling = 1.3
          |    }
          |  ]
          |}""".stripMargin
      )
      val config =
        fixedFeedInModelConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, _) =>
        configs.size shouldBe 2
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )
        configs.contains(
          UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
        )

        actual.getOrDefault[FixedFeedInRuntimeConfig](
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        ) shouldBe actual.getOrDefault[FixedFeedInRuntimeConfig](
          UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
        )
      }
    }

    "setup on multiple correct fixed feed in input configs correctly" in {
      val fixedFeedInModelConfig = ConfigFactory.parseString(
        """simona.runtime.participant.fixedFeedIn = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.3
          |    }
          |  individualConfigs = [
          |    {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
          |    scaling = 1.3
          |  },
          |  {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["fb8f1443-1843-4ecd-a94a-59be8148397f"]
          |    scaling = 1.5
          |  }
          |  ]
          |}""".stripMargin
      )
      val config =
        fixedFeedInModelConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, _) =>
        configs.size shouldBe 2
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )
        configs.contains(
          UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
        )
      }

      actual.getOrDefault[FixedFeedInRuntimeConfig](
        UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        FixedFeedInRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          1.3,
          List("49f250fa-41ff-4434-a083-79c98d260a76"),
        )
      actual.getOrDefault[FixedFeedInRuntimeConfig](
        UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
      ) shouldBe
        FixedFeedInRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          1.5,
          List("fb8f1443-1843-4ecd-a94a-59be8148397f"),
        )
    }
  }

  "The participant config util" should {
    "return default config when the requested config type does not match the found uuid" in {
      val combinedParticipantConfig = ConfigFactory.parseString(
        """simona.runtime.participant.fixedFeedIn = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.0
          |    }
          |  individualConfigs = [
          |    {
          |     calculateMissingReactivePowerWithModel = false
          |     uuids = ["50f250fa-41ff-4434-a083-79c98d260a76"]
          |     scaling = 1.3
          |  },
          |  {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["e7cb5fa7-e4e6-4228-861c-f5b11e88ad1e"]
          |    scaling = 1.5
          |  }
          |  ]
          |}
          |simona.runtime.participant.load = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.0
          |    modelBehaviour = "profile"
          |    reference = "power"
          |    }
          |  individualConfigs = [
          |    {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
          |    scaling = 1.3
          |    modelBehaviour = "profile"
          |    reference = "power"
          |  },
          |  {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["fb8f1443-1843-4ecd-a94a-59be8148397f"]
          |    scaling = 1.5
          |    modelBehaviour = "profile"
          |    reference = "power"
          |  }
          |  ]
          |}
          |simona.runtime.participant.pv = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.0
          |    }
          |  individualConfigs = [
          |   {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
          |    scaling = 1.3
          |  },
          |  {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["fb8f1443-1843-4ecd-a94a-59be8148397f"]
          |    scaling = 1.5
          |  }
          |  ]
          |}""".stripMargin
      )
      val config =
        combinedParticipantConfig.withFallback(typesafeConfig).resolve()
      val simonaConfig = SimonaConfig(config)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.simona.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, _) =>
        configs.size shouldBe 4
      }

      // return default if a request for fix feed is done, but a load config is found
      actual.getOrDefault[FixedFeedInRuntimeConfig](
        UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        FixedFeedInRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          1.0,
          List("default"),
        )

      // return default if a request for load is done, but fixed feed is found
      actual.getOrDefault[LoadRuntimeConfig](
        UUID.fromString("50f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        LoadRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          1.0,
          List("default"),
          "profile",
        )

      // return default if a request for pv is done, but fixed feed is found
      actual.getOrDefault[PvRuntimeConfig](
        UUID.fromString("50f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        PvRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          1.0,
          List("default"),
        )
    }
  }

  "The grid output config util" should {
    "return the correct result entity classes to consider" in {
      val ddt: TableFor2[GridOutputConfig, Set[Class[? <: ResultEntity]]] =
        Table(
          ("config", "expected"),
          (
            new GridOutputConfig(
              false, false, false, false, false, false,
            ),
            Set.empty[Class[? <: ResultEntity]],
          ),
          (
            new GridOutputConfig(
              true, false, false, false, false, false,
            ),
            Set(classOf[CongestionResult]),
          ),
          (
            new GridOutputConfig(
              false, true, false, false, false, false,
            ),
            Set(classOf[LineResult]),
          ),
          (
            new GridOutputConfig(
              false, false, true, false, false, false,
            ),
            Set(classOf[NodeResult]),
          ),
          (
            new GridOutputConfig(
              false, false, false, true, false, false,
            ),
            Set(classOf[SwitchResult]),
          ),
          (
            new GridOutputConfig(
              false, false, false, false, true, false,
            ),
            Set(classOf[Transformer2WResult]),
          ),
          (
            new GridOutputConfig(
              false, false, false, false, false, true,
            ),
            Set(classOf[Transformer3WResult]),
          ),
          (
            new GridOutputConfig(true, true, true, true, true, true),
            Set(
              classOf[CongestionResult],
              classOf[LineResult],
              classOf[NodeResult],
              classOf[SwitchResult],
              classOf[Transformer2WResult],
              classOf[Transformer3WResult],
            ),
          ),
        )

      forAll(ddt) {
        (config: GridOutputConfig, expected: Set[Class[? <: ResultEntity]]) =>
          val actual =
            GridOutputConfigUtil(config).simulationResultEntitiesToConsider
          actual shouldBe expected
      }
    }
  }

  "The participant model output config util" should {
    val validInput = AssetConfigs(
      OutputConfig.ParticipantOutputConfig(
        notifier = "default"
      ),
      List(
        OutputConfig.ParticipantOutputConfig(
          notifier = "load"
        ),
        OutputConfig.ParticipantOutputConfig(
          notifier = "pv"
        ),
      ),
    )

    "build the correct map on valid input" in {
      val configUtil = OutputConfigUtil.participants(validInput)
      inside(configUtil) { case OutputConfigUtil(default, configs) =>
        default shouldBe NotifierConfig(
          simulationResultInfo = false,
          powerRequestReply = false,
          flexResult = false,
        )
        configs shouldBe Map(
          Load -> NotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false,
            flexResult = false,
          ),
          PvPlant -> NotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false,
            flexResult = false,
          ),
        )
      }
    }

    val configUtil = OutputConfigUtil.participants(validInput)
    "return the correct config on request" in {
      val actual = configUtil.getOrDefault(PvPlant)
      actual shouldBe NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false,
        flexResult = false,
      )
    }

    "return default config, when the requested model type is not apparent" in {
      configUtil.getOrDefault(Wec) shouldBe NotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false,
        flexResult = false,
      )
    }

    "return the correct notifier identifiers when the default is to inform about new simulation results" in {
      val inputConfig = AssetConfigs(
        OutputConfig.ParticipantOutputConfig(
          notifier = "default",
          simulationResult = true,
        ),
        List(
          OutputConfig.ParticipantOutputConfig(
            notifier = "load",
            powerRequestReply = true,
            simulationResult = true,
          ),
          OutputConfig.ParticipantOutputConfig(
            notifier = "pv",
            powerRequestReply = true,
          ),
        ),
      )
      val configUtil = OutputConfigUtil.participants(inputConfig)
      val expectedResult: Set[Value] =
        NotifierIdentifier.getParticipantIdentifiers -- Vector(
          NotifierIdentifier.PvPlant
        )

      configUtil.simulationResultIdentifiersToConsider(
        false
      ) shouldBe expectedResult
    }

    "return the correct notifier identifiers when the default is to NOT inform about new simulation results" in {
      val inputConfig = AssetConfigs(
        OutputConfig.ParticipantOutputConfig(
          notifier = "default"
        ),
        List(
          OutputConfig.ParticipantOutputConfig(
            notifier = "load",
            powerRequestReply = true,
            simulationResult = true,
          ),
          OutputConfig.ParticipantOutputConfig(
            notifier = "pv",
            powerRequestReply = true,
          ),
        ),
      )
      val configUtil = OutputConfigUtil.participants(inputConfig)
      val expectedResult: Set[Value] = Set(NotifierIdentifier.Load)

      configUtil.simulationResultIdentifiersToConsider(
        false
      ) shouldBe expectedResult
    }

    "return the correct result entity classes to be considered " in {
      val inputConfig = AssetConfigs(
        OutputConfig.ParticipantOutputConfig(
          notifier = "default"
        ),
        List(
          OutputConfig.ParticipantOutputConfig(
            notifier = "load",
            powerRequestReply = true,
            simulationResult = true,
          ),
          OutputConfig.ParticipantOutputConfig(
            notifier = "pv",
            powerRequestReply = true,
          ),
        ),
      )
      val configUtil = OutputConfigUtil.participants(inputConfig)
      val expectedResult = Set[Class[? <: ResultEntity]](classOf[LoadResult])

      configUtil.simulationResultEntitiesToConsider(
        false
      ) shouldBe expectedResult
    }
  }

  "The database config util" should {
    "throw an exception if kafka is configured with a malformed UUID" in {
      intercept[InvalidConfigParameterException] {
        ConfigUtil.DatabaseConfigUtil.checkKafkaParams(
          ResultKafkaParams(
            "server:1234",
            0,
            "-not-a-uuid-",
            "https://reg:123",
            "topic",
          ),
          Seq("topic"),
        )
      }.getMessage shouldBe "The UUID '-not-a-uuid-' cannot be parsed as it is invalid."
    }

    "throw an exception if kafka is configured, but creating kafka client fails" in {
      intercept[InvalidConfigParameterException] {
        ConfigUtil.DatabaseConfigUtil.checkKafkaParams(
          ResultKafkaParams(
            "not#a#server",
            0,
            "00000000-0000-0000-0000-000000000000",
            "https://reg:123",
            "topic",
          ),
          Seq("topic"),
        )
      }.getMessage shouldBe "Exception creating kafka client for broker not#a#server."
    }

    "throw an exception if kafka is configured, but connection to broker fails" in {
      intercept[InvalidConfigParameterException] {
        ConfigUtil.DatabaseConfigUtil.checkKafkaParams(
          ResultKafkaParams(
            "localhost:12345",
            0,
            "00000000-0000-0000-0000-000000000000",
            "https://reg:123",
            "topic",
          ),
          Seq("topic"),
        )
      }.getMessage shouldBe "Connection with kafka broker localhost:12345 failed."
    }
  }
}
