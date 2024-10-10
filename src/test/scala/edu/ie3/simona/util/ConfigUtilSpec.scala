/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
  Transformer3WResult
}
import edu.ie3.datamodel.models.result.system.{ChpResult, LoadResult}
import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.simona.config.IoConfigUtils.ResultKafkaParams
import edu.ie3.simona.config.OutputConfig.{
  BaseOutputConfig,
  GridOutputConfig,
  ParticipantOutputConfig
}
import edu.ie3.simona.config.RuntimeConfig.{
  LoadRuntimeConfig,
  SimpleRuntimeConfig
}
import edu.ie3.simona.config.SimonaConfig.{apply => _, _}
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.ConfigUtil.NotifierIdentifier._
import edu.ie3.simona.util.ConfigUtil.{
  BaseOutputConfigUtil,
  GridOutputConfigUtil,
  NotifierIdentifier,
  ParticipantConfigUtil
}
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

import java.util.UUID

class ConfigUtilSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with ConfigTestData {
  "The participant config util" should {
    "setup correctly with valid load data" in {
      val loadRuntimeConfig =
        """simona.runtime.participant.load = {
          |  defaultConfig = {
          |    uuids = ["default"]
          |    scaling = 1.0
          |    modelBehaviour = "fix"
          |    reference = "power"
          |    }
          |  individualConfigs = []
          |}""".stripMargin
      val simonaConfig = read_conf_with_fallback(loadRuntimeConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs shouldBe Map.empty[UUID, LoadRuntimeConfig]
        defaultConfigs.size shouldBe 5

        inside(defaultConfigs.get(classOf[LoadRuntimeConfig])) {
          case Some(
                LoadRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids,
                  modelBehaviour,
                  reference
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
      val loadRuntimeConfig =
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
      val simonaConfig = read_conf_with_fallback(loadRuntimeConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs.size shouldBe 1
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )

        defaultConfigs.size shouldBe 5
        inside(defaultConfigs.get(classOf[LoadRuntimeConfig])) {
          case Some(
                LoadRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids,
                  modelBehaviour,
                  reference
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
      val loadRuntimeConfig =
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
      val simonaConfig = read_conf_with_fallback(loadRuntimeConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
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
      val loadRuntimeConfig =
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
      val simonaConfig = read_conf_with_fallback(loadRuntimeConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
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
          uuids = Seq("49f250fa-41ff-4434-a083-79c98d260a76"),
          scaling = 1.3,
          modelBehaviour = "profile",
          reference = "power"
        )
      actual.getOrDefault[LoadRuntimeConfig](
        UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
      ) shouldBe
        LoadRuntimeConfig(
          calculateMissingReactivePowerWithModel = false,
          scaling = 1.5,
          uuids = Seq("fb8f1443-1843-4ecd-a94a-59be8148397f"),
          modelBehaviour = "random",
          reference = "energy"
        )
    }
  }

  "The participant config util" should {
    "setup on correct fixed feed in input data correctly" in {
      val fixedFeedInModelConfig =
        """simona.runtime.participant.fixedFeedIn = {
          |  defaultConfig = {
          |    calculateMissingReactivePowerWithModel = false
          |    uuids = ["default"]
          |    scaling = 1.0
          |    }
          |  individualConfigs = []
          |}""".stripMargin
      val simonaConfig = read_conf_with_fallback(fixedFeedInModelConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs shouldBe Map
          .empty[UUID, LoadRuntimeConfig]

        inside(defaultConfigs.get(classOf[SimpleRuntimeConfig])) {
          case Some(
                SimpleRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids
                )
              ) =>
            calculateMissingReactivePowerWithModel shouldBe false
            scaling shouldBe 1.0
            uuids shouldBe List("default")
          case unexpected =>
            fail(
              s"Expected a default $SimpleRuntimeConfig. I got '$unexpected"
            )
        }
      }
    }

    "setup on one correct fixed feed in input config with one UUID correctly" in {
      val fixedFeedInModelConfig =
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
      val simonaConfig = read_conf_with_fallback(fixedFeedInModelConfig)
      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, defaultConfigs) =>
        configs.size shouldBe 1
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )

        defaultConfigs.size shouldBe 5
        inside(defaultConfigs.get(classOf[SimpleRuntimeConfig])) {
          case Some(
                SimpleRuntimeConfig(
                  calculateMissingReactivePowerWithModel,
                  scaling,
                  uuids
                )
              ) =>
            calculateMissingReactivePowerWithModel shouldBe false
            scaling shouldBe 1.3
            uuids shouldBe List("default")
          case unexpected =>
            fail(
              s"Expected a default $SimpleRuntimeConfig. I got '$unexpected"
            )
        }
      }
    }

    "setup on one correct fixed feed in input config with multiple UUIDs correctly" in {
      val fixedFeedInModelConfig =
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
      val simonaConfig = read_conf_with_fallback(fixedFeedInModelConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, _) =>
        configs.size shouldBe 2
        configs.contains(
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        )
        configs.contains(
          UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
        )

        actual.getOrDefault[SimpleRuntimeConfig](
          UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
        ) shouldBe actual.getOrDefault[SimpleRuntimeConfig](
          UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
        )
      }
    }

    "setup on multiple correct fixed feed in input configs correctly" in {
      val fixedFeedInModelConfig =
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
      val simonaConfig = read_conf_with_fallback(fixedFeedInModelConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
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

      actual.getOrDefault[SimpleRuntimeConfig](
        UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        SimpleRuntimeConfig(
          Seq("49f250fa-41ff-4434-a083-79c98d260a76"),
          1.3,
          calculateMissingReactivePowerWithModel = false
        )
      actual.getOrDefault[SimpleRuntimeConfig](
        UUID.fromString("fb8f1443-1843-4ecd-a94a-59be8148397f")
      ) shouldBe
        SimpleRuntimeConfig(
          Seq("fb8f1443-1843-4ecd-a94a-59be8148397f"),
          1.5,
          calculateMissingReactivePowerWithModel = false
        )
    }
  }

  "The participant config util" should {
    "return default config when the requested config type does not match the found uuid" in {
      val combinedParticipantConfig =
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
      val simonaConfig = read_conf_with_fallback(combinedParticipantConfig)

      val actual = ConfigUtil.ParticipantConfigUtil(
        simonaConfig.runtime.participant
      )

      inside(actual) { case ParticipantConfigUtil(configs, _) =>
        configs.size shouldBe 4
      }

      // return default if a request for fix feed is done, but a load config is found
      actual.getOrDefault[SimpleRuntimeConfig](
        UUID.fromString("49f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        SimpleRuntimeConfig(
          Seq("default"),
          1.0,
          calculateMissingReactivePowerWithModel = false
        )

      // return default if a request for load is done, but fixed feed is found
      actual.getOrDefault[LoadRuntimeConfig](
        UUID.fromString("50f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        LoadRuntimeConfig(
          List("default"),
          1.0,
          calculateMissingReactivePowerWithModel = false,
          "profile",
          "power"
        )

      // return default if a request for pv is done, but fixed feed is found
      actual.getOrDefault[SimpleRuntimeConfig](
        UUID.fromString("50f250fa-41ff-4434-a083-79c98d260a76")
      ) shouldBe
        SimpleRuntimeConfig(
          List("default"),
          1.0,
          calculateMissingReactivePowerWithModel = false
        )
    }
  }

  "The grid output config util" should {
    "return the correct result entity classes to consider" in {
      val ddt: TableFor2[GridOutputConfig, Set[Class[_ <: ResultEntity]]] =
        Table(
          ("config", "expected"),
          (
            GridOutputConfig(
              "grid",
              nodes = false,
              lines = false,
              switches = false,
              transformers2w = false,
              transformers3w = false
            ),
            Set.empty[Class[_ <: ResultEntity]]
          ),
          (
            GridOutputConfig(
              "grid",
              nodes = false,
              switches = false,
              transformers2w = false,
              transformers3w = false
            ),
            Set(classOf[LineResult])
          ),
          (
            GridOutputConfig(
              "grid",
              lines = false,
              switches = false,
              transformers2w = false,
              transformers3w = false
            ),
            Set(classOf[NodeResult])
          ),
          (
            GridOutputConfig(
              "grid",
              nodes = false,
              lines = false,
              transformers2w = false,
              transformers3w = false
            ),
            Set(classOf[SwitchResult])
          ),
          (
            GridOutputConfig(
              "grid",
              nodes = false,
              lines = false,
              switches = false,
              transformers3w = false
            ),
            Set(classOf[Transformer2WResult])
          ),
          (
            GridOutputConfig(
              "grid",
              nodes = false,
              lines = false,
              switches = false,
              transformers2w = false
            ),
            Set(classOf[Transformer3WResult])
          ),
          (
            GridOutputConfig(
              "grid"
            ),
            Set(
              classOf[LineResult],
              classOf[NodeResult],
              classOf[SwitchResult],
              classOf[Transformer2WResult],
              classOf[Transformer3WResult]
            )
          )
        )

      forAll(ddt) {
        (config: GridOutputConfig, expected: Set[Class[_ <: ResultEntity]]) =>
          val actual =
            GridOutputConfigUtil(config).simulationResultEntitiesToConsider
          actual shouldBe expected
      }
    }
  }

  "The participant model output config util" should {
    val validInput = ParticipantOutputConfig(
      BaseOutputConfig(
        notifier = "default",
        powerRequestReply = false,
        simulationResult = false
      ),
      Seq(
        BaseOutputConfig(
          notifier = "load",
          powerRequestReply = false,
          simulationResult = false
        ),
        BaseOutputConfig(
          notifier = "pv",
          powerRequestReply = false,
          simulationResult = false
        ),
        BaseOutputConfig(
          notifier = "chp",
          powerRequestReply = false,
          simulationResult = false
        )
      )
    )

    "build the correct map on valid input" in {
      val configUtil = BaseOutputConfigUtil(validInput)
      inside(configUtil) { case BaseOutputConfigUtil(default, configs) =>
        default shouldBe ParticipantNotifierConfig(
          simulationResultInfo = false,
          powerRequestReply = false
        )
        configs shouldBe Map(
          Load -> ParticipantNotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          ),
          PvPlant -> ParticipantNotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          ),
          ChpPlant -> ParticipantNotifierConfig(
            simulationResultInfo = false,
            powerRequestReply = false
          )
        )
      }
    }

    val configUtil = BaseOutputConfigUtil(validInput)
    "return the correct config on request" in {
      val actual = configUtil.getOrDefault(PvPlant)
      actual shouldBe ParticipantNotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false
      )
    }

    "return default config, when the requested model type is not apparent" in {
      configUtil.getOrDefault(Wec) shouldBe ParticipantNotifierConfig(
        simulationResultInfo = false,
        powerRequestReply = false
      )
    }

    "return the correct notifier identifiers when the default is to inform about new simulation results" in {
      val inputConfig = ParticipantOutputConfig(
        BaseOutputConfig(
          notifier = "default",
          powerRequestReply = false,
          simulationResult = true
        ),
        Seq(
          BaseOutputConfig(
            notifier = "load",
            powerRequestReply = true,
            simulationResult = true
          ),
          BaseOutputConfig(
            notifier = "pv",
            powerRequestReply = true,
            simulationResult = false
          ),
          BaseOutputConfig(
            notifier = "chp",
            powerRequestReply = true,
            simulationResult = true
          )
        )
      )
      val configUtil = BaseOutputConfigUtil(inputConfig)
      val expectedResult: Set[Value] = NotifierIdentifier.values -- Vector(
        NotifierIdentifier.PvPlant
      )

      configUtil.simulationResultIdentifiersToConsider shouldBe expectedResult
    }

    "return the correct notifier identifiers when the default is to NOT inform about new simulation results" in {
      val inputConfig = ParticipantOutputConfig(
        BaseOutputConfig(
          notifier = "default",
          powerRequestReply = false,
          simulationResult = false
        ),
        Seq(
          BaseOutputConfig(
            notifier = "load",
            powerRequestReply = true,
            simulationResult = true
          ),
          BaseOutputConfig(
            notifier = "pv",
            powerRequestReply = true,
            simulationResult = false
          ),
          BaseOutputConfig(
            notifier = "chp",
            powerRequestReply = true,
            simulationResult = true
          )
        )
      )
      val configUtil = BaseOutputConfigUtil(inputConfig)
      val expectedResult: Set[Value] =
        Set(NotifierIdentifier.Load, NotifierIdentifier.ChpPlant)

      configUtil.simulationResultIdentifiersToConsider shouldBe expectedResult
    }

    "return the correct result entity classes to be considered " in {
      val inputConfig = ParticipantOutputConfig(
        BaseOutputConfig(
          notifier = "default",
          powerRequestReply = false,
          simulationResult = false
        ),
        Seq(
          BaseOutputConfig(
            notifier = "load",
            powerRequestReply = true,
            simulationResult = true
          ),
          BaseOutputConfig(
            notifier = "pv",
            powerRequestReply = true,
            simulationResult = false
          ),
          BaseOutputConfig(
            notifier = "chp",
            powerRequestReply = true,
            simulationResult = true
          )
        )
      )
      val configUtil = BaseOutputConfigUtil(inputConfig)
      val expectedResult: Set[Class[_ <: ResultEntity]] =
        Set[Class[_ <: ResultEntity]](classOf[LoadResult], classOf[ChpResult])

      configUtil.simulationResultEntitiesToConsider shouldBe expectedResult
    }
  }

  "The database config util" should {
    "throw an exception if kafka is configured with a malformed UUID" in {
      intercept[InvalidConfigParameterException] {
        ConfigUtil.DatabaseConfigUtil.checkKafkaParams(
          ResultKafkaParams(
            "-not-a-uuid-",
            "server:1234",
            "https://reg:123",
            0,
            "topic"
          ),
          Seq("topic")
        )
      }.getMessage shouldBe "The UUID '-not-a-uuid-' cannot be parsed as it is invalid."
    }

    "throw an exception if kafka is configured, but creating kafka client fails" in {
      intercept[InvalidConfigParameterException] {
        ConfigUtil.DatabaseConfigUtil.checkKafkaParams(
          ResultKafkaParams(
            "00000000-0000-0000-0000-000000000000",
            "not#a#server",
            "https://reg:123",
            0,
            "topic"
          ),
          Seq("topic")
        )
      }.getMessage shouldBe "Exception creating kafka client for broker not#a#server."
    }

    "throw an exception if kafka is configured, but connection to broker fails" in {
      intercept[InvalidConfigParameterException] {
        ConfigUtil.DatabaseConfigUtil.checkKafkaParams(
          ResultKafkaParams(
            "00000000-0000-0000-0000-000000000000",
            "localhost:12345",
            "https://reg:123",
            0,
            "topic"
          ),
          Seq("topic")
        )
      }.getMessage shouldBe "Connection with kafka broker localhost:12345 failed."
    }
  }
}
