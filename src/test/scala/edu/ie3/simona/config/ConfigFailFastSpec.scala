/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.InputConfig.{
  GridDataSource,
  WeatherDataSourceConfig,
  WeatherSampleParams
}
import edu.ie3.simona.config.IoConfigUtils.{
  BaseCsvParams,
  InfluxDb1xParams,
  PsdmCsvParams,
  ResultKafkaParams
}
import edu.ie3.simona.config.OutputConfig.{
  BaseOutputConfig,
  OutputCsvParams,
  OutputSinkConfig
}
import edu.ie3.simona.config.RuntimeConfig.{
  LoadRuntimeConfig,
  SimpleRuntimeConfig
}
import edu.ie3.simona.config.SimonaConfig.{
  NewtonRaphsonConfig,
  PowerFlowConfig,
  TimeConfig
}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.ConfigUtil.{CsvConfigUtil, NotifierIdentifier}
import edu.ie3.util.TimeUtil

import scala.concurrent.duration.DurationInt
import java.time.ZonedDateTime

class ConfigFailFastSpec extends UnitSpec with ConfigTestData {
  "Validating the configs" when {
    "validating the simona config" when {
      "Checking date input" should {
        val checkTimeConfig = PrivateMethod[Unit](Symbol("checkTimeConfig"))

        "let valid input pass" in {
          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkTimeConfig(
              TimeConfig(
                "2020-05-18 13:41:00",
                "2020-06-18 13:41:00",
                stopOnFailedPowerFlow = true,
                None
              )
            )
          }
        }

        "identify invalid date or time configuration" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTimeConfig(
              TimeConfig(
                "2020-07-18 13:41:00",
                "2020-06-18 13:41:00",
                stopOnFailedPowerFlow = true,
                None
              )
            )
          }.getMessage shouldBe "Invalid time configuration." +
            "Please ensure that the start time of the simulation is before the end time."
        }
      }

      "Checking date string" should {
        val createDateTime =
          PrivateMethod[ZonedDateTime](Symbol("createDateTime"))

        val dateTimeString: String = "2020-05-18 13:41:00"

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
            "Please ensure that your date/time parameter match the following pattern: 'yyyy-MM-dd HH:mm:ss'"
        }
      }

      "Checking power flow resolution" should {
        val checkPowerFlowResolutionConfiguration =
          PrivateMethod[Unit](Symbol("checkPowerFlowResolutionConfiguration"))

        "let valid input pass" in {
          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPowerFlowResolutionConfiguration(
              PowerFlowConfig(
                10,
                3600.seconds,
                NewtonRaphsonConfig(
                  List(10, 30),
                  100
                ),
                3600.seconds
              )
            )
          }
        }

        "identify invalid input" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkPowerFlowResolutionConfiguration(
              PowerFlowConfig(
                10,
                3600.seconds,
                NewtonRaphsonConfig(
                  List(10, 30),
                  100
                ),
                3600.nanos
              )
            )
          }.getMessage shouldBe "Invalid time resolution. Please ensure, that the time resolution for power flow calculation is at least rounded to a full second!"
        }
      }

      "A configuration with faulty refSystem parameters" should {
        val checkRefSystem = PrivateMethod[Unit](Symbol("checkRefSystem"))

        "throw an InvalidConfigParametersException when gridIds and voltLvls are empty" in {
          val refSystemConfigAllEmpty =
            "simona.gridConfig.refSystems = [{sNom=\"100 MVA\", vNom=\"0.4 kV\"}]"
          val faultySimonaConfig =
            read_conf_with_fallback(refSystemConfigAllEmpty)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast.check(faultySimonaConfig)
          }.getMessage startsWith "The provided values for voltLvls and gridIds are empty! " +
            "At least one of these optional parameters has to be provided for a valid refSystem! " +
            "Provided refSystem is: "
        }

        "throw an InvalidConfigParametersException when the gridId is malformed" in {

          val malformedGridIds = List("10--100", "MS", "10..100")

          malformedGridIds.foreach(malformedGridId => {

            val refSystemConfigAllEmpty =
              s"""simona.gridConfig.refSystems = [
                   |  {
                   |   sNom="100 MVA",
                   |   vNom="0.4 kV",
                   |   gridIds = [$malformedGridId]
                   |   }
                   |]""".stripMargin
            val faultySimonaConfig =
              read_conf_with_fallback(refSystemConfigAllEmpty)

            intercept[InvalidConfigParameterException] {
              faultySimonaConfig.gridConfig.refSystems.foreach(refSystem =>
                ConfigFailFast invokePrivate checkRefSystem(refSystem)
              )
            }.getMessage shouldBe s"The provided gridId $malformedGridId is malformed!"
          })
        }

        "throw an InvalidConfigParameterException if the nominal voltage of the voltage level is malformed" in {

          val refSystemConfigAllEmpty =
            """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100 MVA",
                |   vNom="0.4 kV",
                |   voltLvls = [{id = "1", vNom = "foo"}]
                |   }
                |]""".stripMargin
          val faultySimonaConfig =
            read_conf_with_fallback(refSystemConfigAllEmpty)

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.gridConfig.refSystems.foreach(refSystem =>
              ConfigFailFast invokePrivate checkRefSystem(refSystem)
            )
          }.getMessage shouldBe "The given nominal voltage 'foo' cannot be parsed to a quantity. Did you provide the volt level with it's unit (e.g. \"20 kV\")?"

        }

        "throw an InvalidConfigParametersException when sNom is invalid" in {
          val refSystemConfigAllEmpty =
            """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100",
                |   vNom="0.4 kV",
                |   voltLvls = [{id = "MS", vNom = "10 kV"},{id = "HS", vNom = "110 kV"}]
                |   }
                |]""".stripMargin
          val faultySimonaConfig =
            read_conf_with_fallback(refSystemConfigAllEmpty)

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.gridConfig.refSystems.foreach(refSystem =>
              ConfigFailFast invokePrivate checkRefSystem(refSystem)
            )
          }.getMessage startsWith "Invalid value for sNom from provided refSystem RefSystemConfig"

        }

        "throw an InvalidConfigParametersException when vNom is invalid" in {

          val refSystemConfigAllEmpty =
            """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100 MVA",
                |   vNom="0.4",
                |   voltLvls = [{id = "MS", vNom = "10 kV"},{id = "HS", vNom = "110 kV"}]
                |   }
                |]""".stripMargin
          val faultySimonaConfig =
            read_conf_with_fallback(refSystemConfigAllEmpty)

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.gridConfig.refSystems.foreach(refSystem =>
              ConfigFailFast invokePrivate checkRefSystem(refSystem)
            )
          }.getMessage startsWith "Invalid value for vNom from provided refSystem RefSystemConfig"

        }

        "work as expected for correctly provided data" in {
          val refSystemConfigAllEmpty =
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
          val simonaConfig = read_conf_with_fallback(refSystemConfigAllEmpty)

          simonaConfig.gridConfig.refSystems.foreach(refSystem => {
            ConfigFailFast invokePrivate checkRefSystem(refSystem)
          })

        }
      }

      "Checking a participant model config" should {
        val checkParticipantRuntimeConfiguration =
          PrivateMethod[Unit](Symbol("checkParticipantRuntimeConfiguration"))

        "throw an InvalidConfigParameterException, if the participant power request voltage deviation threshold is negative" in {
          val participantModelConfig =
            "simona.runtime.participant.requestVoltageDeviationThreshold = -1E-10"
          val simonaConfig = read_conf_with_fallback(participantModelConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.runtime.participant
            )
          }.getMessage shouldBe "The participant power request voltage deviation threshold must be positive!"
        }

        "let a valid uniform config pass if only default config is given" in {
          val participantModelConfig =
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
          val simonaConfig = read_conf_with_fallback(participantModelConfig)

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.runtime.participant
            )
          }
        }

        "let a valid config with specific load and fixed feed in model config pass" in {
          val loadModelConfig =
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
          val simonaConfig = read_conf_with_fallback(loadModelConfig)

          val checkParticipantRuntimeConfiguration =
            PrivateMethod[Unit](Symbol("checkParticipantRuntimeConfiguration"))

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.runtime.participant
            )
          }
        }

        "let a valid list of load and fixed feed in individual configs pass" in {
          val loadModelConfig =
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
          val simonaConfig = read_conf_with_fallback(loadModelConfig)

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkParticipantRuntimeConfiguration(
              simonaConfig.runtime.participant
            )
          }
        }
      }

      "Checking a runtime model config" should {
        // get the private method for validation
        val checkBaseRuntimeConfigs =
          PrivateMethod[LoadRuntimeConfig](Symbol("checkBaseRuntimeConfigs"))

        val defaultString: String = "default"

        "throw an InvalidConfigParameterException if the list of UUIDs of the base model config is empty" in {
          val baseRuntimeConfig =
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

          val simonaConfig = read_conf_with_fallback(baseRuntimeConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.runtime.participant.load,
              defaultString
            )
          }.getMessage shouldBe "There has to be at least one identifier for each participant."
        }

        "throw an InvalidConfigParameterException if a valid single key is given and the UUID of the base model config is not valid" in {
          val baseRuntimeConfig =
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
          val simonaConfig = read_conf_with_fallback(baseRuntimeConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.runtime.participant.load,
              defaultString
            )
          }.getMessage shouldBe "Found invalid UUID 'blabla' it was meant to be the string 'default' or a valid UUID."
        }

        "throw an InvalidConfigParameterException if the UUID of the base model config is not valid" in {
          val baseRuntimeConfig =
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
          val simonaConfig = read_conf_with_fallback(baseRuntimeConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.runtime.participant.load,
              defaultString
            )
          }.getMessage shouldBe s"Found invalid UUID 'blabla' it was meant to be the string 'default' or a valid UUID."
        }

        "throw an InvalidConfigParameterException if the scaling factor of the load model config is negative" in {
          val baseRuntimeConfig =
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
          val simonaConfig = read_conf_with_fallback(baseRuntimeConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.runtime.participant.load,
              defaultString
            )
          }.getMessage shouldBe "The scaling factor for system participants with UUID '49f250fa-41ff-4434-a083-79c98d260a76' may not be negative."
        }

        "throw an InvalidConfigParameterException if there is an ambiguous model configuration" in {
          val baseRuntimeConfig =
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
          val simonaConfig = read_conf_with_fallback(baseRuntimeConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.runtime.participant.load,
              defaultString
            )
          }.getMessage shouldBe "The basic model configurations contain ambiguous definitions."
        }
      }

      "Checking specific load model configs" should {
        // get the private method for validation
        val checkSpecificLoadModelConfig =
          PrivateMethod[Unit](Symbol("checkSpecificLoadModelConfig"))

        "throw an InvalidConfigParameterException if the model behaviour of the load model config is not supported" in {

          val loadModelConfig =
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
          val simonaConfig = read_conf_with_fallback(loadModelConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkSpecificLoadModelConfig(
              simonaConfig.runtime.participant.load.defaultConfig
            )
          }.getMessage shouldBe "The load model behaviour 'blabla' for the loads with UUIDs '49f250fa-41ff-4434-a083-79c98d260a76' is invalid."
        }

        "throw an InvalidConfigParameterException if the load profile reference of the load model config is not supported" in {
          val loadModelConfig =
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
          val simonaConfig = read_conf_with_fallback(loadModelConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkSpecificLoadModelConfig(
              simonaConfig.runtime.participant.load.defaultConfig
            )
          }.getMessage shouldBe "The standard load profile reference 'blabla' for the loads with UUIDs '49f250fa-41ff-4434-a083-79c98d260a76' is invalid."
        }

      }

      "Checking runtime listener configs" should {
        val checkRuntimeListenerConfiguration =
          PrivateMethod[Unit](Symbol("checkRuntimeListenerConfiguration"))

        "throw an exception if kafka is configured, but connection to broker fails" in {
          val runtimeListenerConfig =
            """simona.runtime.listener.kafka {
              |  topic = "topic"
              |  runId = "00000000-0000-0000-0000-000000000000"
              |  bootstrapServers = "localhost:12345"
              |  schemaRegistryUrl = "https://reg:123"
              |  linger = 3
              |}""".stripMargin
          val simonaConfig = read_conf_with_fallback(runtimeListenerConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkRuntimeListenerConfiguration(
              simonaConfig.runtime.listener
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
            BaseOutputConfig(
              notifier = "load",
              powerRequestReply = true,
              simulationResult = false
            ),
            BaseOutputConfig(
              notifier = "pv",
              powerRequestReply = true,
              simulationResult = false
            ),
            BaseOutputConfig(
              notifier = "chp",
              powerRequestReply = true,
              simulationResult = false
            )
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkIndividualParticipantsOutputConfigs(
              validInput
            )
          }
        }

        "throw an exception, when there is a duplicate entry for the same model type" in {
          val invalidInput = List(
            BaseOutputConfig(
              notifier = "load",
              powerRequestReply = true,
              simulationResult = false
            ),
            BaseOutputConfig(
              notifier = "pv",
              powerRequestReply = true,
              simulationResult = false
            ),
            BaseOutputConfig(
              notifier = "load",
              powerRequestReply = false,
              simulationResult = true
            )
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
            ConfigFailFast invokePrivate checkDataSink(
              OutputSinkConfig(None, None, None)
            )
          }.getLocalizedMessage shouldBe "No sink configuration found! Please ensure that at least " +
            "one sink is configured! You can choose from: influxdb1x, csv, kafka."
        }

        "throw an exception if more than one sink is provided" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkDataSink(
              OutputSinkConfig(
                Some(OutputCsvParams()),
                Some(InfluxDb1xParams("", 0, "")),
                None
              )
            )
          }.getLocalizedMessage shouldBe "Multiple sink configurations are not supported! Please ensure that only " +
            "one sink is configured!"
        }

        "throw an exception if an influxDb1x is configured, but not accessible" ignore {
          intercept[java.lang.IllegalArgumentException] {
            ConfigFailFast invokePrivate checkDataSink(
              OutputSinkConfig(None, Some(InfluxDb1xParams("", 0, "")), None)
            )
          }.getLocalizedMessage shouldBe "Unable to reach configured influxDb1x with url ':0' for 'Sink' configuration and database ''. " +
            "Exception: java.lang.IllegalArgumentException: Unable to parse url: :0"
        }

        "throw an exception if kafka is configured, but connection to broker fails" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkDataSink(
              OutputSinkConfig(
                None,
                None,
                Some(
                  ResultKafkaParams(
                    "00000000-0000-0000-0000-000000000000",
                    "localhost:12345",
                    "https://reg:123",
                    0,
                    "topic"
                  )
                )
              )
            )
          }.getMessage shouldBe "Connection with kafka broker localhost:12345 failed."
        }
      }

      "Checking grid data sources" should {
        "identify a faulty csv separator" in {
          val csvParams =
            BaseCsvParams("inputData/test", "\t")

          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData"
            )
          }.getMessage shouldBe "The csvSep parameter '\t' for 'CsvGridData' configuration is invalid! Please choose between ';' or ','!"
        }

        "identify a an empty path" in {
          val csvParams = BaseCsvParams("", ",")
          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData"
            )
          }.getMessage shouldBe "The provided directoryPath for .csv-files '' for 'CsvGridData' configuration is invalid! Please correct the path!"
        }

        "identify a non-existing path" in {
          val csvParams =
            BaseCsvParams("somewhere/else", ",")

          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData"
            )
          }.getMessage shouldBe "The provided directoryPath for .csv-files 'somewhere/else' for 'CsvGridData' configuration is invalid! Please correct the path!"
        }

        "identify a path pointing to a file" in {
          val csvParams = BaseCsvParams(
            "inputData/common/akka.conf",
            ","
          )

          intercept[InvalidConfigParameterException] {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData"
            )
          }.getMessage shouldBe "The provided directoryPath for .csv-files 'inputData/common/akka.conf' for 'CsvGridData' configuration is invalid! Please correct the path!"
        }

        "let valid csv parameters pass" in {
          val csvParams =
            BaseCsvParams("input/samples/vn_simona", ",")
          noException shouldBe thrownBy {
            CsvConfigUtil.checkBaseCsvParams(
              csvParams,
              "CsvGridData"
            )
          }
        }

        val checkGridDataSource =
          PrivateMethod[Unit](Symbol("checkGridDataSource"))

        "identify grid data source with empty id" in {
          val gridDataSource = GridDataSource(
            id = "",
            csvParams = Some(
              PsdmCsvParams(",", "inputData/vn_simona", isHierarchic = false)
            )
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "No grid data source information provided! Cannot proceed!"
        }

        "identify unsupported id" in {
          val gridDataSource = GridDataSource(
            id = "someWhereUndefined",
            None
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "The provided grid data source 'someWhereUndefined' is not supported!"
        }

        "identify missing csv parameters" in {
          val gridDataSource = GridDataSource(
            id = "csv",
            None
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "No grid data source csv parameters provided. If you intend to read grid data from " +
            ".csv-files, please provide .csv parameters!"
        }

        "let valid csv grid data source definition pass" in {
          val gridDataSource = GridDataSource(
            id = "csv",
            Some(
              PsdmCsvParams(
                "input/samples/vn_simona",
                ",",
                isHierarchic = false
              )
            )
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
            WeatherDataSourceConfig(
              "this won't work",
              Some(
                WeatherSampleParams(true)
              ),
              Some("yyyy-MM-dd HH:mm"),
              Some(360L),
              50000d,
              None,
              None,
              None,
              None,
              InputConfig.CoordinateSourceConfig(
                "icon",
                None,
                Some(
                  InputConfig.WeatherSampleParams(true)
                ),
                None
              )
            )
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkWeatherDataSource(
              weatherDataSource
            )
          }.getMessage shouldBe "The weather data scheme 'this won't work' is not supported. Supported schemes:\n\ticon\n\tcosmo"
        }

      }
    }

    "validating the typesafe config" when {
      "checking the availability of akka logger parameterization" should {
        val checkAkkaLoggers = PrivateMethod[Unit](Symbol("checkAkkaLoggers"))

        "log warning on malicious config" in {
          val maliciousConfig = ConfigFactory.parseString("")

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkAkkaLoggers(maliciousConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }

        "pass on proper config" in {
          val properConfig = ConfigFactory.parseString(
            """
              |akka {
              |  loggers = ["akka.event.slf4j.Slf4jLogger"]
              |}""".stripMargin
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkAkkaLoggers(properConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }
      }

      "checking the akka config" should {
        val checkAkkaConfig = PrivateMethod[Unit](Symbol("checkAkkaConfig"))

        "log warning on missing entry" in {
          val maliciousConfig = ConfigFactory.parseString("")

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkAkkaConfig(maliciousConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }

        "pass on proper config" in {
          val properConfig = ConfigFactory.parseString(
            """
              |akka {
              |  loggers = ["akka.event.slf4j.Slf4jLogger"]
              |}""".stripMargin
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkAkkaConfig(properConfig)
          }
          /* Testing the log message cannot be tested, as with LazyLogging, the logger of the class cannot be spied. */
        }
      }

      "checking the overall config" should {
        "pass on proper input" in {
          val properConfig = ConfigFactory.parseString(
            """
              |akka {
              |  loggers = ["akka.event.slf4j.Slf4jLogger"]
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
            "akka.loggers = [\"akka.event.slf4j.Slf4jLogger\"]"
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
