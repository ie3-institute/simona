/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
import edu.ie3.simona.config.SimonaConfig.Simona.Output.Sink
import edu.ie3.simona.config.SimonaConfig.Simona.Output.Sink.{Csv, InfluxDb1x}
import edu.ie3.simona.config.SimonaConfig.Simona.Powerflow.Newtonraphson
import edu.ie3.simona.config.SimonaConfig.Simona.{Powerflow, Time}
import edu.ie3.simona.config.SimonaConfig.{
  BaseCsvParams,
  ResultKafkaParams,
  TransformerControlGroup,
}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.ConfigUtil.{CsvConfigUtil, NotifierIdentifier}
import edu.ie3.util.TimeUtil

import java.time.temporal.ChronoUnit
import java.time.{Duration, ZonedDateTime}

class ConfigFailFastSpec extends UnitSpec with ConfigTestData {
  "Validating the configs" when {
    "validating the simona config" when {
      "Checking date input" should {
        val checkTimeConfig = PrivateMethod[Unit](Symbol("checkTimeConfig"))

        "let valid input pass" in {
          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkTimeConfig(
              new Time(
                "2020-06-18T13:41:00Z",
                None,
                "2020-05-18T13:41:00Z",
              )
            )
          }
        }

        "identify invalid date or time configuration" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTimeConfig(
              new Time(
                "2020-06-18T13:41:00Z",
                None,
                "2020-07-18T13:41:00Z",
              )
            )
          }.getMessage shouldBe "Invalid time configuration." +
            "Please ensure that the start time of the simulation is before the end time."
        }
      }

      "Checking date string" should {
        val createDateTime =
          PrivateMethod[ZonedDateTime](Symbol("createDateTime"))

        val dateTimeString: String = "2020-05-18T13:41:00Z"

        "let valid input pass" in {

          ConfigFailFast invokePrivate createDateTime(
            dateTimeString
          ) shouldBe TimeUtil.withDefaults.toZonedDateTime(dateTimeString)

        }

        "identify invalid input" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate createDateTime(
              "total non-sense"
            )
          }.getMessage shouldBe "Invalid dateTimeString: total non-sense." +
            "Please ensure that your date/time parameter match the following pattern: 'yyyy-MM-dd'T'HH:mm:ss'Z''"
        }
      }

      "Checking power flow resolution" should {
        val checkPowerFlowResolutionConfiguration =
          PrivateMethod[Unit](Symbol("checkPowerFlowResolutionConfiguration"))

        "let valid input pass" in {
          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPowerFlowResolutionConfiguration(
              new Powerflow(
                10,
                new Newtonraphson(
                  List(10, 30),
                  100,
                ),
                Duration.of(3600, ChronoUnit.SECONDS),
                stopOnFailure = false,
                Duration.of(3600, ChronoUnit.SECONDS),
              )
            )
          }
        }

        "identify invalid input" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkPowerFlowResolutionConfiguration(
              new Powerflow(
                10,
                new Newtonraphson(
                  List(10, 30),
                  100,
                ),
                resolution = Duration.of(3600, ChronoUnit.NANOS),
                stopOnFailure = false,
                sweepTimeout = Duration.of(3600, ChronoUnit.SECONDS),
              )
            )
          }.getMessage shouldBe "Invalid time resolution. Please ensure, that the time resolution for power flow calculation is at least rounded to a full second!"
        }
      }

      "A configuration with faulty refSystem parameters" should {
        val checkRefSystem = PrivateMethod[Unit](Symbol("checkRefSystem"))

        "throw an InvalidConfigParametersException when gridIds and voltLvls are empty" in {

          val refSystemConfigAllEmpty = ConfigFactory.parseString(
            "simona.gridConfig.refSystems = [{sNom=\"100 MVA\", vNom=\"0.4 kV\"}]"
          )
          val faultyConfig =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig = SimonaConfig(faultyConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast.check(faultySimonaConfig)
          }.getMessage shouldBe "The provided values for voltLvls and gridIds are empty! " +
            "At least one of these optional parameters has to be provided for a valid refSystem! " +
            "Provided refSystem is: RefSystemConfig(None,100 MVA,0.4 kV,None)."

        }

        "throw an InvalidConfigParametersException when the gridId is malformed" in {

          val malformedGridIds = List("10--100", "MS", "10..100")

          malformedGridIds.foreach(malformedGridId => {

            val refSystemConfigAllEmpty =
              ConfigFactory.parseString(s"""simona.gridConfig.refSystems = [
                   |  {
                   |   sNom="100 MVA",
                   |   vNom="0.4 kV",
                   |   gridIds = [$malformedGridId]
                   |   }
                   |]""".stripMargin)
            val faultyConfig =
              refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
            val faultySimonaConfig = SimonaConfig(faultyConfig)

            intercept[InvalidConfigParameterException] {
              faultySimonaConfig.simona.gridConfig.refSystems.foreach(
                refSystem =>
                  ConfigFailFast invokePrivate checkRefSystem(refSystem)
              )
            }.getMessage shouldBe s"The provided gridId $malformedGridId is malformed!"

          })
        }

        "throw an InvalidConfigParameterException if the nominal voltage of the voltage level is malformed" in {

          val refSystemConfigAllEmpty =
            ConfigFactory.parseString("""simona.gridConfig.refSystems = [
                |  {
                |   sNom="100 MVA",
                |   vNom="0.4 kV",
                |   voltLvls = [{id = "1", vNom = "foo"}]
                |   }
                |]""".stripMargin)
          val faultyConfig =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig = SimonaConfig(faultyConfig)

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.simona.gridConfig.refSystems.foreach(refSystem =>
              ConfigFailFast invokePrivate checkRefSystem(refSystem)
            )
          }.getMessage shouldBe "The given nominal voltage 'foo' cannot be parsed to a quantity. Did you provide the volt level with it's unit (e.g. \"20 kV\")?"

        }

        "throw an InvalidConfigParametersException when sNom is invalid" in {
          val refSystemConfigAllEmpty =
            ConfigFactory.parseString(
              """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100",
                |   vNom="0.4 kV",
                |   voltLvls = [{id = "MS", vNom = "10 kV"},{id = "HS", vNom = "110 kV"}]
                |   }
                |]""".stripMargin
            )
          val faultyConfig =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig = SimonaConfig(faultyConfig)

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.simona.gridConfig.refSystems.foreach(refSystem =>
              ConfigFailFast invokePrivate checkRefSystem(refSystem)
            )
          }.getMessage shouldBe "Invalid value for sNom from provided refSystem RefSystemConfig(None,100,0.4 kV,Some(List(VoltLvlConfig(MS,10 kV), VoltLvlConfig(HS,110 kV)))). Is a valid unit provided?"

        }

        "throw an InvalidConfigParametersException when vNom is invalid" in {

          val refSystemConfigAllEmpty =
            ConfigFactory.parseString(
              """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100 MVA",
                |   vNom="0.4",
                |   voltLvls = [{id = "MS", vNom = "10 kV"},{id = "HS", vNom = "110 kV"}]
                |   }
                |]""".stripMargin
            )
          val faultyConfig =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig = SimonaConfig(faultyConfig)

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.simona.gridConfig.refSystems.foreach(refSystem =>
              ConfigFailFast invokePrivate checkRefSystem(refSystem)
            )
          }.getMessage shouldBe "Invalid value for vNom from provided refSystem RefSystemConfig(None,100 MVA,0.4,Some(List(VoltLvlConfig(MS,10 kV), VoltLvlConfig(HS,110 kV)))). Is a valid unit provided?"

        }

        "work as expected for correctly provided data" in {
          val refSystemConfigAllEmpty =
            ConfigFactory.parseString(
              """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100 MVA",
                |   vNom="0.4 kV",
                |   voltLvls = [{id = "MS", vNom = "10 kV"},{id = "HS", vNom = "110 kV"}]
                |   gridIds = ["1","1-10","10...100"]
                |   },
                |   {
                |   sNom="1000 MVA",
                |   vNom="10kV",
                |   voltLvls = [{id = "HS", vNom = "110 kV"},{id = "HoeS", vNom = "380 kV"}]
                |   gridIds = ["1-3","3...6","10...100"]
                |   }
                |]""".stripMargin
            )
          val config =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          simonaConfig.simona.gridConfig.refSystems.foreach(refSystem => {
            ConfigFailFast invokePrivate checkRefSystem(refSystem)
          })

        }
      }

      "Checking a participant model config" should {
        val checkParticipantRuntimeConfiguration =
          PrivateMethod[Unit](Symbol("checkParticipantRuntimeConfiguration"))

        "throw an InvalidConfigParameterException, if the participant power request voltage deviation threshold is negative" in {
          val participantModelConfig = ConfigFactory.parseString(
            "simona.runtime.participant.requestVoltageDeviationThreshold = -1E-10"
          )
          val config =
            participantModelConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.simona.runtime.participant
            )
          }.getMessage shouldBe "The participant power request voltage deviation threshold must be positive!"
        }

        "let a valid uniform config pass if only default config is given" in {
          val participantModelConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    uuids = ["default"]
              |    scaling = 1.0
              |    modelBehaviour = "fix"
              |    reference = "power"
              |  }
              |  individualConfigs = []
              |}
              |
              |simona.runtime.participant.fixedFeedIn = {
              |  defaultConfig = {
              |     uuids = ["default"]
              |     scaling = 1.0
              |  }
              |  individualConfigs = []
              |}""".stripMargin
          )
          val config =
            participantModelConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.simona.runtime.participant
            )
          }
        }

        "let a valid config with specific load and fixed feed in model config pass" in {
          val loadModelConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["default"]
              |    scaling = 1.0
              |    modelBehaviour = "fix"
              |    reference = "power"
              |  }
              |  individualConfigs = [
              |    {
              |     calculateMissingReactivePowerWithModel = false
              |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |      scaling = 1.3
              |      modelBehaviour = "profile"
              |      reference = "power"
              |    }
              |  ]
              |}
              |
              |simona.runtime.participant.fixedFeedIn = {
              |  defaultConfig = {
              |       calculateMissingReactivePowerWithModel = false
              |       uuids = ["default"]
              |       scaling = 1.0
              |  }
              |  individualConfigs = [
              |    {
              |       calculateMissingReactivePowerWithModel = false
              |       uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |       scaling = 1.3
              |    }
              |  ]
              |}""".stripMargin
          )
          val config =
            loadModelConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          val checkParticipantRuntimeConfiguration =
            PrivateMethod[Unit](Symbol("checkParticipantRuntimeConfiguration"))

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.simona.runtime.participant
            )
          }
        }

        "let a valid list of load and fixed feed in individual configs pass" in {
          val loadModelConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["default"]
              |    scaling = 1.0
              |    modelBehaviour = "fix"
              |    reference = "power"
              |  }
              |  individualConfigs = [
              |    {
              |     calculateMissingReactivePowerWithModel = false
              |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |      scaling = 1.3
              |      modelBehaviour = "profile"
              |      reference = "power"
              |    },
              |    {
              |     calculateMissingReactivePowerWithModel = false
              |      uuids = ["fb8f1443-1843-4ecd-a94a-59be8148397f"]
              |      scaling = 1.5
              |      modelBehaviour = "random"
              |      reference = "energy"
              |      }
              |  ]
              |}
              |
              |simona.runtime.participant.fixedFeedIn = {
              |  defaultConfig = {
              |     calculateMissingReactivePowerWithModel = false
              |     uuids = ["default"]
              |     scaling = 1.0
              |  }
              |  individualConfigs = [
              |    {
              |       calculateMissingReactivePowerWithModel = false
              |       uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |       scaling = 1.3
              |    },
              |    {
              |      calculateMissingReactivePowerWithModel = false
              |      uuids = ["fb8f1443-1843-4ecd-a94a-59be8148397f"]
              |      scaling = 1.5
              |    }
              |  ]
              |}""".stripMargin
          )
          val config =
            loadModelConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.simona.runtime.participant
            )
          }
        }
      }

      "Checking a runtime model config" should {
        // get the private method for validation
        val checkBaseRuntimeConfigs =
          PrivateMethod[Unit](Symbol("checkBaseRuntimeConfigs"))

        val defaultString: String = "default"

        "throw an InvalidConfigParameterException if the list of UUIDs of the base model config is empty" in {
          val baseRuntimeConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = []
              |    scaling = 1.3
              |    modelBehaviour = "profile"
              |    reference = "power"
              |  }
              |  individualConfigs = []
              |}""".stripMargin
          )
          val config =
            baseRuntimeConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.simona.runtime.participant.load.defaultConfig,
              simonaConfig.simona.runtime.participant.load.individualConfigs,
              defaultString,
            )
          }.getMessage shouldBe "There has to be at least one identifier for each participant."
        }

        "throw an InvalidConfigParameterException if a valid single key is given and the UUID of the base model config is not valid" in {
          val baseRuntimeConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["blabla"]
              |    scaling = 1.3
              |    modelBehaviour = "profile"
              |    reference = "power"
              |  }
              |  individualConfigs = []
              |}""".stripMargin
          )
          val config =
            baseRuntimeConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.simona.runtime.participant.load.defaultConfig,
              simonaConfig.simona.runtime.participant.load.individualConfigs,
              defaultString,
            )
          }.getMessage shouldBe "Found invalid UUID 'blabla' it was meant to be the string 'default' or a valid UUID."
        }

        "throw an InvalidConfigParameterException if the UUID of the base model config is not valid" in {
          val baseRuntimeConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["blabla"]
              |    scaling = 1.3
              |    modelBehaviour = "profile"
              |    reference = "power"
              |  }
              |  individualConfigs = []
              |}""".stripMargin
          )
          val config =
            baseRuntimeConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.simona.runtime.participant.load.defaultConfig,
              simonaConfig.simona.runtime.participant.load.individualConfigs,
              defaultString,
            )
          }.getMessage shouldBe s"Found invalid UUID 'blabla' it was meant to be the string 'default' or a valid UUID."
        }

        "throw an InvalidConfigParameterException if the scaling factor of the load model config is negative" in {
          val baseRuntimeConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |    scaling = -5.3
              |    modelBehaviour = "profile"
              |    reference = "power"
              |  }
              |  individualConfigs = []
              |}""".stripMargin
          )
          val config =
            baseRuntimeConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.simona.runtime.participant.load.defaultConfig,
              simonaConfig.simona.runtime.participant.load.individualConfigs,
              defaultString,
            )
          }.getMessage shouldBe "The scaling factor for system participants with UUID '49f250fa-41ff-4434-a083-79c98d260a76' may not be negative."
        }

        "throw an InvalidConfigParameterException if there is an ambiguous model configuration" in {
          val baseRuntimeConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["default"]
              |    scaling = 1.0
              |    modelBehaviour = "fix"
              |    reference = "power"
              |  }
              |  individualConfigs = [
              |    {
              |      calculateMissingReactivePowerWithModel = false
              |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |      scaling = 1.3
              |      modelBehaviour = "profile"
              |      reference = "power"
              |    },
              |    {
              |      calculateMissingReactivePowerWithModel = false
              |      uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |      scaling = 1.5
              |      modelBehaviour = "random"
              |      reference = "energy"
              |      }
              |  ]
              |}""".stripMargin
          )
          val config =
            baseRuntimeConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.simona.runtime.participant.load.defaultConfig,
              simonaConfig.simona.runtime.participant.load.individualConfigs,
              defaultString,
            )
          }.getMessage shouldBe "The basic model configurations contain ambiguous definitions."
        }
      }

      "Checking specific load model configs" should {
        // get the private method for validation
        val checkSpecificLoadModelConfig =
          PrivateMethod[Unit](Symbol("checkSpecificLoadModelConfig"))

        "throw an InvalidConfigParameterException if the model behaviour of the load model config is not supported" in {

          val loadModelConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |    scaling = 1.3
              |    modelBehaviour = "blabla"
              |    reference = "power"
              |  }
              |  individualConfigs = []
              |}""".stripMargin
          )
          val config =
            loadModelConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkSpecificLoadModelConfig(
              simonaConfig.simona.runtime.participant.load.defaultConfig
            )
          }.getMessage shouldBe "The load model behaviour 'blabla' for the loads with UUIDs '49f250fa-41ff-4434-a083-79c98d260a76' is invalid."
        }

        "throw an InvalidConfigParameterException if the load profile reference of the load model config is not supported" in {
          val loadModelConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  defaultConfig = {
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = ["49f250fa-41ff-4434-a083-79c98d260a76"]
              |    scaling = 1.3
              |    modelBehaviour = "profile"
              |    reference = "blabla"
              |  }
              |  individualConfigs = []
              |}""".stripMargin
          )
          val config =
            loadModelConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkSpecificLoadModelConfig(
              simonaConfig.simona.runtime.participant.load.defaultConfig
            )
          }.getMessage shouldBe "The standard load profile reference 'blabla' for the loads with UUIDs '49f250fa-41ff-4434-a083-79c98d260a76' is invalid."
        }

      }

      "Checking runtime listener configs" should {
        val checkRuntimeListenerConfiguration =
          PrivateMethod[Unit](Symbol("checkRuntimeListenerConfiguration"))

        "throw an exception if kafka is configured, but connection to broker fails" in {
          val runtimeListenerConfig = ConfigFactory.parseString(
            """simona.runtime.listener.kafka {
              |  topic = "topic"
              |  runId = "00000000-0000-0000-0000-000000000000"
              |  bootstrapServers = "localhost:12345"
              |  schemaRegistryUrl = "https://reg:123"
              |  linger = 3
              |}""".stripMargin
          )
          val config =
            runtimeListenerConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkRuntimeListenerConfiguration(
              simonaConfig.simona.runtime.listener
            )
          }.getMessage shouldBe "Connection with kafka broker localhost:12345 failed."
        }
      }

      "Checking participant output configs" should {
        val checkNotifierIdentifier =
          PrivateMethod[Unit](Symbol("checkNotifierIdentifier"))

        "identify faulty notifier identifiers" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkNotifierIdentifier("whatever")
          }.getMessage shouldBe s"The identifier 'whatever' you provided is not valid. Valid input: ${NotifierIdentifier.values.map(_.toString).mkString(",")}"
        }

        "let all valid notifier identifiers pass" in {
          noException shouldBe thrownBy {
            NotifierIdentifier.values.map(id =>
              ConfigFailFast invokePrivate checkNotifierIdentifier(
                id.toString
              )
            )
          }
        }

        val checkIndividualParticipantsOutputConfigs =
          PrivateMethod[Unit](
            Symbol("checkIndividualParticipantsOutputConfigs")
          )

        "let distinct configs pass" in {
          val validInput = List(
            SimonaConfig.ParticipantBaseOutputConfig(
              notifier = "load",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
            SimonaConfig.ParticipantBaseOutputConfig(
              notifier = "pv",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
            SimonaConfig.ParticipantBaseOutputConfig(
              notifier = "chp",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkIndividualParticipantsOutputConfigs(
              validInput
            )
          }
        }

        "throw an exception, when there is a duplicate entry for the same model type" in {
          val invalidInput = List(
            SimonaConfig.ParticipantBaseOutputConfig(
              notifier = "load",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
            SimonaConfig.ParticipantBaseOutputConfig(
              notifier = "pv",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
            SimonaConfig.ParticipantBaseOutputConfig(
              notifier = "load",
              powerRequestReply = false,
              simulationResult = true,
              flexResult = false,
            ),
          )

          intercept[InvalidConfigParameterException](
            ConfigFailFast invokePrivate checkIndividualParticipantsOutputConfigs(
              invalidInput
            )
          ).getMessage shouldBe "There are multiple output configurations for participant types 'load'."
        }
      }

      "Checking data sinks" should {
        val checkDataSink = PrivateMethod[Unit](Symbol("checkDataSink"))

        "throw an exception if no sink is provided" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkDataSink(Sink(None, None, None))
          }.getLocalizedMessage shouldBe "No sink configuration found! Please ensure that at least " +
            "one sink is configured! You can choose from: influxdb1x, csv, kafka."
        }

        "throw an exception if more than one sink is provided" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkDataSink(
              Sink(
                Some(Csv("", "", "", isHierarchic = false)),
                Some(InfluxDb1x("", 0, "")),
                None,
              )
            )
          }.getLocalizedMessage shouldBe "Multiple sink configurations are not supported! Please ensure that only " +
            "one sink is configured!"
        }

        "throw an exception if an influxDb1x is configured, but not accessible" ignore {
          intercept[java.lang.IllegalArgumentException] {
            ConfigFailFast invokePrivate checkDataSink(
              Sink(None, Some(InfluxDb1x("", 0, "")), None)
            )
          }.getLocalizedMessage shouldBe "Unable to reach configured influxDb1x with url ':0' for 'Sink' configuration and database ''. " +
            "Exception: java.lang.IllegalArgumentException: Unable to parse url: :0"
        }

        "throw an exception if kafka is configured, but connection to broker fails" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkDataSink(
              Sink(
                None,
                None,
                Some(
                  ResultKafkaParams(
                    "localhost:12345",
                    0,
                    "00000000-0000-0000-0000-000000000000",
                    "https://reg:123",
                    "topic",
                  )
                ),
              )
            )
          }.getMessage shouldBe "Connection with kafka broker localhost:12345 failed."
        }
      }

      "Checking grid data sources" should {
        "identify a faulty csv separator" in {
          val csvParams =
            BaseCsvParams("\t", "inputData/test", isHierarchic = false)

          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData",
            )
          }.getMessage shouldBe "The csvSep parameter '\t' for 'CsvGridData' configuration is invalid! Please choose between ';' or ','!"
        }

        "identify a an empty path" in {
          val csvParams = BaseCsvParams(",", "", isHierarchic = false)
          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData",
            )
          }.getMessage shouldBe "The provided directoryPath for .csv-files '' for 'CsvGridData' configuration is invalid! Please correct the path!"
        }

        "identify a non-existing path" in {
          val csvParams =
            BaseCsvParams(",", "somewhere/else", isHierarchic = false)

          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData",
            )
          }.getMessage shouldBe "The provided directoryPath for .csv-files 'somewhere/else' for 'CsvGridData' configuration is invalid! Please correct the path!"
        }

        "identify a path pointing to a file" in {
          val csvParams = BaseCsvParams(
            ",",
            "inputData/common/pekko.conf",
            isHierarchic = false,
          )

          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData",
            )
          }.getMessage shouldBe "The provided directoryPath for .csv-files 'inputData/common/pekko.conf' for 'CsvGridData' configuration is invalid! Please correct the path!"
        }

        "let valid csv parameters pass" in {
          val csvParams =
            BaseCsvParams(",", "input/samples/vn_simona", isHierarchic = false)
          noException shouldBe thrownBy {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData",
            )
          }
        }

        val checkGridDataSource =
          PrivateMethod[Unit](Symbol("checkGridDataSource"))

        "identify grid data source with empty id" in {
          val gridDataSource = SimonaConfig.Simona.Input.Grid.Datasource(
            Some(
              BaseCsvParams(",", "inputData/vn_simona", isHierarchic = false)
            ),
            id = "",
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "No grid data source information provided! Cannot proceed!"
        }

        "identify unsupported id" in {
          val gridDataSource = SimonaConfig.Simona.Input.Grid.Datasource(
            None,
            id = "someWhereUndefined",
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "The provided grid data source 'someWhereUndefined' is not supported!"
        }

        "identify missing csv parameters" in {
          val gridDataSource = SimonaConfig.Simona.Input.Grid.Datasource(
            None,
            id = "csv",
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "No grid data source csv parameters provided. If you intend to read grid data from " +
            ".csv-files, please provide .csv parameters!"
        }

        "let valid csv grid data source definition pass" in {
          val gridDataSource = SimonaConfig.Simona.Input.Grid.Datasource(
            Some(
              BaseCsvParams(
                ",",
                "input/samples/vn_simona",
                isHierarchic = false,
              )
            ),
            id = "csv",
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }
        }
      }

      /* Checking of primary source configuration is delegated to the specific actor. Tests are placed there */

      "Checking weather data sources" should {

        val checkWeatherDataSource =
          PrivateMethod[Unit](Symbol("checkWeatherDataSource"))

        "detects invalid weather data scheme" in {
          val weatherDataSource =
            new SimonaConfig.Simona.Input.Weather.Datasource(
              CoordinateSource(
                None,
                "icon",
                Some(
                  SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
                    .SampleParams(true)
                ),
                None,
              ),
              None,
              None,
              None,
              50000d,
              Some(360L),
              Some(
                SimonaConfig.Simona.Input.Weather.Datasource.SampleParams(true)
              ),
              "this won't work",
              None,
              Some("yyyy-MM-dd HH:mm"),
            )
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkWeatherDataSource(
              weatherDataSource
            )
          }.getMessage shouldBe "The weather data scheme 'this won't work' is not supported. Supported schemes:\n\ticon\n\tcosmo"
        }
      }

      "checking the transformer control groups" should {
        val checkTransformerControl =
          PrivateMethod[Unit](Symbol("checkTransformerControl"))

        "throw an exception, if the measurements are empty" in {
          val dut = TransformerControlGroup(
            List.empty,
            List("a16cf7ca-8bbf-46e1-a74e-ffa6513c89a8"),
            1.02,
            0.98,
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTransformerControl(dut)
          }.getMessage shouldBe s"A transformer control group (${dut.toString}) cannot have no measurements assigned."
        }

        "throw an exception, if the transformers are empty" in {
          val dut = TransformerControlGroup(
            List("6888c53a-7629-4563-ac8e-840f80b03106"),
            List.empty,
            1.02,
            0.98,
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTransformerControl(dut)
          }.getMessage shouldBe s"A transformer control group (${dut.toString}) cannot have no transformers assigned."
        }

        "throw an exception, if vMax is smaller than vMin" in {
          val dut = TransformerControlGroup(
            List("6888c53a-7629-4563-ac8e-840f80b03106"),
            List("a16cf7ca-8bbf-46e1-a74e-ffa6513c89a8"),
            0.98,
            1.02,
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTransformerControl(dut)
          }.getMessage shouldBe s"The minimum permissible voltage magnitude of a transformer control group (${dut.toString}) must be smaller than the maximum permissible voltage magnitude."
        }

        "throw Exception if vMin is lower than -20% of nominal Voltage" in {
          val dut = TransformerControlGroup(
            List("6888c53a-7629-4563-ac8e-840f80b03106"),
            List("a16cf7ca-8bbf-46e1-a74e-ffa6513c89a8"),
            1.02,
            0.79,
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTransformerControl(dut)
          }.getMessage shouldBe s"A control group (${dut.toString}) which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
            "by invalid parametrization of one control groups where vMin is lower than the lower boundary (0.8 of nominal Voltage)!"
        }

        "throw Exception if vMax is higher than +20% of nominal Voltage" in {
          val dut = TransformerControlGroup(
            List("6888c53a-7629-4563-ac8e-840f80b03106"),
            List("a16cf7ca-8bbf-46e1-a74e-ffa6513c89a8"),
            1.21,
            0.98,
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTransformerControl(dut)
          }.getMessage shouldBe s"A control group (${dut.toString}) which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
            "by invalid parametrization of one control groups where vMax is higher than the upper boundary (1.2 of nominal Voltage)!"
        }
      }
    }

    "checking the parameterization of storages" should {
      val checkStorageConfigs =
        PrivateMethod[Unit](Symbol("checkStoragesConfig"))

      "throw exception if default initial SOC is negative" in {

        val defaultConfig: SimonaConfig.StorageRuntimeConfig =
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            -0.5,
            Some(0.8),
          )
        val storageConfig = SimonaConfig.Simona.Runtime.Participant
          .Storage(defaultConfig, List.empty)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe "StorageRuntimeConfig: Default initial SOC needs to be between 0.0 and 1.0."
      }

      "throw exception if default target SOC is negative" in {
        val defaultConfig: SimonaConfig.StorageRuntimeConfig =
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(-0.8),
          )
        val storageConfig = SimonaConfig.Simona.Runtime.Participant
          .Storage(defaultConfig, List.empty)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe "StorageRuntimeConfig: Default target SOC needs to be between 0.0 and 1.0."
      }

      "throw exception if individual initial SOC is negative" in {
        val uuid = java.util.UUID.randomUUID().toString
        val defaultConfig: SimonaConfig.StorageRuntimeConfig =
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        val individualConfig: List[SimonaConfig.StorageRuntimeConfig] = List(
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(uuid),
            -0.5,
            Some(0.8),
          )
        )
        val storageConfig = SimonaConfig.Simona.Runtime.Participant
          .Storage(defaultConfig, individualConfig)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe s"StorageRuntimeConfig: List($uuid) initial SOC needs to be between 0.0 and 1.0."
      }

      "throw exception if individual target SOC is negative" in {
        val uuid = java.util.UUID.randomUUID().toString
        val defaultConfig: SimonaConfig.StorageRuntimeConfig =
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        val individualConfig: List[SimonaConfig.StorageRuntimeConfig] = List(
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(uuid),
            0.5,
            Some(-0.8),
          )
        )
        val storageConfig = SimonaConfig.Simona.Runtime.Participant
          .Storage(defaultConfig, individualConfig)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe s"StorageRuntimeConfig: List($uuid) target SOC needs to be between 0.0 and 1.0."
      }

      "not throw exception if all parameters are in parameter range" in {
        val defaultConfig: SimonaConfig.StorageRuntimeConfig =
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        val individualConfig: List[SimonaConfig.StorageRuntimeConfig] = List(
          SimonaConfig.StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        )
        val storageConfig = SimonaConfig.Simona.Runtime.Participant
          .Storage(defaultConfig, individualConfig)

        noException should be thrownBy {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }
      }
    }

    "validating the typesafe config" when {
      "checking the availability of pekko logger parameterization" should {
        val checkPekkoLoggers = PrivateMethod[Unit](Symbol("checkPekkoLoggers"))

        "log warning on malicious config" in {
          val maliciousConfig = ConfigFactory.parseString("")

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPekkoLoggers(maliciousConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }

        "pass on proper config" in {
          val properConfig = ConfigFactory.parseString(
            """
              |pekko {
              |  loggers = ["pekko.event.slf4j.Slf4jLogger"]
              |}""".stripMargin
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPekkoLoggers(properConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }
      }

      "checking the pekko config" should {
        val checkPekkoConfig = PrivateMethod[Unit](Symbol("checkPekkoConfig"))

        "log warning on missing entry" in {
          val maliciousConfig = ConfigFactory.parseString("")

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPekkoConfig(maliciousConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }

        "pass on proper config" in {
          val properConfig = ConfigFactory.parseString(
            """
              |pekko {
              |  loggers = ["pekko.event.slf4j.Slf4jLogger"]
              |}""".stripMargin
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPekkoConfig(properConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }
      }

      "checking the overall config" should {
        "pass on proper input" in {
          val properConfig = ConfigFactory.parseString(
            """
              |pekko {
              |  loggers = ["pekko.event.slf4j.Slf4jLogger"]
              |}""".stripMargin
          )

          noException shouldBe thrownBy {
            ConfigFailFast.check(properConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }
      }
    }

    "validating both configs together" when {
      "having two proper inputs" should {
        "pass" in {
          val properTypesafeConfig = ConfigFactory.parseString(
            "pekko.loggers = [\"pekko.event.slf4j.Slf4jLogger\"]"
          )
          val properSimonaConfig = simonaConfig

          noException shouldBe thrownBy {
            ConfigFailFast.check(properTypesafeConfig, properSimonaConfig)
          }
        }
      }
    }
  }
}
