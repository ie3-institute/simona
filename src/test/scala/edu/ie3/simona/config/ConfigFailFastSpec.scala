/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.ConfigParams._
import edu.ie3.simona.config.InputConfig.{
  CoordinateSource,
  WeatherDatasource,
  Primary => PrimaryConfig,
}
import edu.ie3.simona.config.OutputConfig.Sink
import edu.ie3.simona.config.RuntimeConfig.StorageRuntimeConfig
import edu.ie3.simona.config.SimonaConfig.Simona.Powerflow.Newtonraphson
import edu.ie3.simona.config.SimonaConfig.Simona.{Powerflow, Time}
import edu.ie3.simona.config.SimonaConfig.{
  AssetConfigs,
  TransformerControlGroup,
}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.util.ConfigUtil.{CsvConfigUtil, NotifierIdentifier}
import edu.ie3.util.TimeUtil

import java.time.ZonedDateTime
import scala.concurrent.duration.DurationInt

class ConfigFailFastSpec extends UnitSpec with ConfigTestData {
  "Validating the configs" when {
    "validating the simona config" when {
      "Checking date input" should {
        val checkTimeConfig = PrivateMethod[Unit](Symbol("checkTimeConfig"))

        "let valid input pass" in {
          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkTimeConfig(
              Time(
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
              Time(
                "2020-06-18T13:41:00Z",
                None,
                "2020-07-18T13:41:00Z",
              )
            )
          }.getMessage shouldBe "Invalid time configuration." +
            "Please ensure that the start time of the simulation is before the end time."
        }

        "identify if start and end date configuration are equal" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkTimeConfig(
              Time(
                "2020-06-18T13:41:00Z",
                None,
                "2020-06-18T13:41:00Z",
              )
            )
          }.getMessage shouldBe "Invalid time configuration." +
            "Please ensure that the start time of the simulation is not equal to the end time."
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
                Newtonraphson(
                  List(10, 30),
                  100,
                ),
                3600.seconds,
                stopOnFailure = false,
                3600.seconds,
              )
            )
          }
        }

        "identify invalid input" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkPowerFlowResolutionConfiguration(
              new Powerflow(
                10,
                Newtonraphson(
                  List(10, 30),
                  100,
                ),
                resolution = 3600.nanos,
                stopOnFailure = false,
                sweepTimeout = 3600.seconds,
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

          val malformedGridIds = List("10--100", "MV", "10..100")

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
            val faultySimonaConfig: List[SimonaConfig] =
              List(SimonaConfig(faultyConfig))

            intercept[InvalidConfigParameterException] {
              faultySimonaConfig.foreach(conf =>
                conf.simona.gridConfig.refSystems.foreach(refSystem =>
                  ConfigFailFast invokePrivate checkRefSystem(refSystem)
                )
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
          val faultySimonaConfig: List[SimonaConfig] =
            List(SimonaConfig(faultyConfig))

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.foreach(conf =>
              conf.simona.gridConfig.refSystems.foreach(refSystem =>
                ConfigFailFast invokePrivate checkRefSystem(refSystem)
              )
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
                |   voltLvls = [{id = "MV", vNom = "10 kV"},{id = "HV", vNom = "110 kV"}]
                |   }
                |]""".stripMargin
            )
          val faultyConfig =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig: List[SimonaConfig] =
            List(SimonaConfig(faultyConfig))

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.foreach(conf =>
              conf.simona.gridConfig.refSystems.foreach(refSystem =>
                ConfigFailFast invokePrivate checkRefSystem(refSystem)
              )
            )
          }.getMessage shouldBe "Invalid value for sNom from provided refSystem RefSystemConfig(None,100,0.4 kV,Some(List(VoltLvlConfig(MV,10 kV), VoltLvlConfig(HV,110 kV)))). Is a valid unit provided?"

        }

        "throw an InvalidConfigParametersException when vNom is invalid" in {

          val refSystemConfigAllEmpty =
            ConfigFactory.parseString(
              """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100 MVA",
                |   vNom="0.4",
                |   voltLvls = [{id = "MV", vNom = "10 kV"},{id = "HV", vNom = "110 kV"}]
                |   }
                |]""".stripMargin
            )
          val faultyConfig =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig: List[SimonaConfig] =
            List(SimonaConfig(faultyConfig))

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.foreach(conf =>
              conf.simona.gridConfig.refSystems.foreach(refSystem =>
                ConfigFailFast invokePrivate checkRefSystem(refSystem)
              )
            )
          }.getMessage shouldBe "Invalid value for vNom from provided refSystem RefSystemConfig(None,100 MVA,0.4,Some(List(VoltLvlConfig(MV,10 kV), VoltLvlConfig(HV,110 kV)))). Is a valid unit provided?"

        }

        "work as expected for correctly provided data" in {
          val refSystemConfigAllEmpty =
            ConfigFactory.parseString(
              """simona.gridConfig.refSystems = [
                |  {
                |   sNom="100 MVA",
                |   vNom="0.4 kV",
                |   voltLvls = [{id = "MV", vNom = "10 kV"},{id = "HV", vNom = "110 kV"}]
                |   gridIds = ["1","1-10","10...100"]
                |   },
                |   {
                |   sNom="1000 MVA",
                |   vNom="10kV",
                |   voltLvls = [{id = "HV", vNom = "110 kV"},{id = "EHV", vNom = "380 kV"}]
                |   gridIds = ["1-3","3...6","10...100"]
                |   }
                |]""".stripMargin
            )
          val config =
            refSystemConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val simonaConfig = List(SimonaConfig(config))

          simonaConfig.foreach(conf =>
            conf.simona.gridConfig.refSystems.foreach(refSystem => {
              ConfigFailFast invokePrivate checkRefSystem(refSystem)
            })
          )

        }
      }

      "A configuration with faulty voltage limit parameters" should {
        val checkVoltageLimits =
          PrivateMethod[Unit](Symbol("checkVoltageLimits"))

        "throw an InvalidConfigParametersException when gridIds and voltLvls are empty" in {

          val voltageLimitsConfigAllEmpty = ConfigFactory.parseString(
            "simona.gridConfig.voltageLimits = [{vMin=\"0.9\", vMax=\"1.1\"}]"
          )
          val faultyConfig =
            voltageLimitsConfigAllEmpty.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig = SimonaConfig(faultyConfig)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast.check(faultySimonaConfig)
          }.getMessage shouldBe "The provided values for voltLvls and gridIds are empty! " +
            "At least one of these optional parameters has to be provided for a valid voltage limit! " +
            "Provided voltage limit is: VoltageLimitsConfig(None,1.1,0.9,None)."

        }

        "throw an InvalidConfigParametersException when the gridId is malformed" in {

          val malformedGridIds = List("10--100", "MV", "10..100")

          malformedGridIds.foreach(malformedGridId => {

            val voltageLimitsConfig =
              ConfigFactory.parseString(s"""simona.gridConfig.voltageLimits = [
                                           |  {
                                           |   vMin="0.9",
                                           |   vMax="1.1",
                                           |   gridIds = [$malformedGridId]
                                           |   }
                                           |]""".stripMargin)
            val faultyConfig =
              voltageLimitsConfig.withFallback(typesafeConfig).resolve()
            val faultySimonaConfig: List[SimonaConfig] =
              List(SimonaConfig(faultyConfig))

            intercept[InvalidConfigParameterException] {
              faultySimonaConfig.foreach(conf =>
                conf.simona.gridConfig.voltageLimits.foreach(refSystem =>
                  ConfigFailFast invokePrivate checkVoltageLimits(refSystem)
                )
              )
            }.getMessage shouldBe s"The provided gridId $malformedGridId is malformed!"

          })
        }

        "throw an InvalidConfigParameterException if the nominal voltage of the voltage level is malformed" in {

          val voltageLimitsConfig =
            ConfigFactory.parseString("""simona.gridConfig.voltageLimits = [
                                        |  {
                                        |   vMin="0.9",
                                        |   vMax="1.1",
                                        |   voltLvls = [{id = "1", vNom = "foo"}]
                                        |   }
                                        |]""".stripMargin)
          val faultyConfig =
            voltageLimitsConfig.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig: List[SimonaConfig] =
            List(SimonaConfig(faultyConfig))

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.foreach(conf =>
              conf.simona.gridConfig.voltageLimits.foreach(refSystem =>
                ConfigFailFast invokePrivate checkVoltageLimits(refSystem)
              )
            )
          }.getMessage shouldBe "The given nominal voltage 'foo' cannot be parsed to a quantity. Did you provide the volt level with it's unit (e.g. \"20 kV\")?"

        }

        "throw an InvalidConfigParametersException when vMin is greater than vMax" in {
          val voltageLimitsConfig =
            ConfigFactory.parseString(
              """simona.gridConfig.voltageLimits = [
                |  {
                |   vMin="1.1",
                |   vMax="1.0",
                |   voltLvls = [{id = "MV", vNom = "10 kV"},{id = "HV", vNom = "110 kV"}]
                |   }
                |]""".stripMargin
            )
          val faultyConfig =
            voltageLimitsConfig.withFallback(typesafeConfig).resolve()
          val faultySimonaConfig: List[SimonaConfig] =
            List(SimonaConfig(faultyConfig))

          intercept[InvalidConfigParameterException] {
            faultySimonaConfig.foreach(conf =>
              conf.simona.gridConfig.voltageLimits.foreach(refSystem =>
                ConfigFailFast invokePrivate checkVoltageLimits(refSystem)
              )
            )
          }.getMessage shouldBe "Invalid value for vMin and vMax from provided voltage limit VoltageLimitsConfig(None,1.0,1.1,Some(List(VoltLvlConfig(MV,10 kV), VoltLvlConfig(HV,110 kV)))). Is vMin smaller than vMax?"
        }

        "work as expected for correctly provided data" in {
          val voltageLimitsConfig =
            ConfigFactory.parseString(
              """simona.gridConfig.voltageLimits = [
                |  {
                |   vMin="0.9",
                |   vMax="1.1",
                |   voltLvls = [{id = "MV", vNom = "10 kV"},{id = "HV", vNom = "110 kV"}]
                |   gridIds = ["1","1-10","10...100"]
                |   },
                |   {
                |   vMin="0.9",
                |   vMax="1.1",
                |   voltLvls = [{id = "HV", vNom = "110 kV"},{id = "EHV", vNom = "380 kV"}]
                |   gridIds = ["1-3","3...6","10...100"]
                |   }
                |]""".stripMargin
            )
          val config =
            voltageLimitsConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = List(SimonaConfig(config))

          simonaConfig.foreach(conf =>
            conf.simona.gridConfig.voltageLimits.foreach(refSystem => {
              ConfigFailFast invokePrivate checkVoltageLimits(refSystem)
            })
          )

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

        "throw an InvalidConfigParameterException if the list of UUIDs of the individual model config is empty" in {
          val baseRuntimeConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  individualConfigs = [{
              |    calculateMissingReactivePowerWithModel = false
              |    uuids = []
              |    scaling = 1.3
              |    modelBehaviour = "profile"
              |    reference = "power"
              |  }]
              |}""".stripMargin
          )
          val config =
            baseRuntimeConfig.withFallback(typesafeConfig).resolve()
          val simonaConfig = SimonaConfig(config)

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkBaseRuntimeConfigs(
              simonaConfig.simona.runtime.participant.load.defaultConfig,
              simonaConfig.simona.runtime.participant.load.individualConfigs,
            )
          }.getMessage shouldBe "There has to be at least one identifier for each participant."
        }

        "throw an InvalidConfigParameterException if the UUID of an individual config is not valid" in {
          val baseRuntimeConfig = ConfigFactory.parseString(
            """simona.runtime.participant.load = {
              |  individualConfigs = [
              |    {
              |      calculateMissingReactivePowerWithModel = false
              |      uuids = ["blabla"]
              |      scaling = 1.3
              |      modelBehaviour = "profile"
              |      reference = "power"
              |    }
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
            )
          }.getMessage shouldBe s"The UUID 'blabla' cannot be parsed as it is invalid."
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
            ConfigFailFast invokePrivate checkNotifierIdentifier(
              "whatever",
              NotifierIdentifier.getParticipantIdentifiers,
            )
          }.getMessage shouldBe s"The identifier 'whatever' you provided is not valid. Valid input: ${NotifierIdentifier.getParticipantIdentifiers.map(_.toString).mkString(",")}"
        }

        "let all valid notifier identifiers pass" in {
          noException shouldBe thrownBy {
            NotifierIdentifier.getParticipantIdentifiers.map(id =>
              ConfigFailFast invokePrivate checkNotifierIdentifier(
                id.toString,
                NotifierIdentifier.getParticipantIdentifiers,
              )
            )
          }
        }

        val checkIndividualOutputConfigs =
          PrivateMethod[Unit](
            Symbol("checkIndividualOutputConfigs")
          )

        "let distinct configs pass" in {
          val validInput = List(
            OutputConfig.ParticipantOutputConfig(
              notifier = "load",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
            OutputConfig.ParticipantOutputConfig(
              notifier = "pv",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkIndividualOutputConfigs(
              validInput,
              "participant",
            )
          }
        }

        "throw an exception, when there is a duplicate entry for the same model type" in {
          val invalidInput = List(
            OutputConfig.ParticipantOutputConfig(
              notifier = "load",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
            OutputConfig.ParticipantOutputConfig(
              notifier = "pv",
              powerRequestReply = true,
              simulationResult = false,
              flexResult = false,
            ),
            OutputConfig.ParticipantOutputConfig(
              notifier = "load",
              powerRequestReply = false,
              simulationResult = true,
              flexResult = false,
            ),
          )

          intercept[InvalidConfigParameterException](
            ConfigFailFast invokePrivate checkIndividualOutputConfigs(
              invalidInput,
              "participant",
            )
          ).getMessage shouldBe "There are multiple output configurations for participant types 'load'."
        }
      }

      "Checking thermal output configs" should {
        val checkNotifierIdentifier =
          PrivateMethod[Unit](Symbol("checkNotifierIdentifier"))

        "identify faulty notifier identifiers" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkNotifierIdentifier(
              "whatever",
              NotifierIdentifier.getThermalIdentifiers,
            )
          }.getMessage shouldBe s"The identifier 'whatever' you provided is not valid. Valid input: ${NotifierIdentifier.getThermalIdentifiers.map(_.toString).mkString(",")}"
        }

        "let all valid notifier identifiers pass" in {
          noException shouldBe thrownBy {
            Set("house", "cylindricalstorage").map(id =>
              ConfigFailFast invokePrivate checkNotifierIdentifier(
                id,
                NotifierIdentifier.getThermalIdentifiers,
              )
            )
          }
        }

        val checkIndividualOutputConfigs =
          PrivateMethod[Unit](
            Symbol("checkIndividualOutputConfigs")
          )

        "let distinct configs pass" in {
          val validInput = List(
            OutputConfig.SimpleOutputConfig(
              notifier = "house",
              simulationResult = false,
            ),
            OutputConfig.SimpleOutputConfig(
              notifier = "cylindricalstorage",
              simulationResult = false,
            ),
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkIndividualOutputConfigs(
              validInput,
              "thermal",
            )
          }
        }

        "throw an exception, when there is a duplicate entry for the same model type" in {
          val invalidInput = List(
            OutputConfig.SimpleOutputConfig(
              notifier = "house",
              simulationResult = false,
            ),
            OutputConfig.SimpleOutputConfig(
              notifier = "cylindricalstorage",
              simulationResult = false,
            ),
            OutputConfig.SimpleOutputConfig(
              notifier = "house",
              simulationResult = false,
            ),
          )

          intercept[InvalidConfigParameterException](
            ConfigFailFast invokePrivate checkIndividualOutputConfigs(
              invalidInput,
              "thermal",
            )
          ).getMessage shouldBe "There are multiple output configurations for thermal types 'house'."
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
                Some(
                  PsdmSinkCsvParams(
                    compressOutputs = false,
                    "",
                    "",
                    "",
                    isHierarchic = false,
                  )
                ),
                Some(BaseInfluxDb1xParams("", 0, "")),
                None,
              )
            )
          }.getLocalizedMessage shouldBe "Multiple sink configurations are not supported! Please ensure that only " +
            "one sink is configured!"
        }

        "throw an exception if an influxDb1x is configured, but not accessible" in {
          intercept[java.lang.IllegalArgumentException] {
            ConfigFailFast invokePrivate checkDataSink(
              Sink(None, Some(BaseInfluxDb1xParams("", 0, "")), None)
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

      "Checking log config" should {
        val checkLogOutputConfig =
          PrivateMethod[Unit](Symbol("checkLogOutputConfig"))

        "identify an unknown log level" in {
          val invalidLogConfig = OutputConfig.Log("INVALID")

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkLogOutputConfig(invalidLogConfig)
          }.getMessage shouldBe "Invalid log level \"INVALID\". Valid log levels: TRACE, DEBUG, INFO, WARN, ERROR"
        }

        "let valid log output configuration pass" in {
          val validLogConfig = OutputConfig.Log("WARN")

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkLogOutputConfig(validLogConfig)
          }
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
          val gridDataSource = InputConfig.GridDatasource(
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
          val gridDataSource = InputConfig.GridDatasource(
            None,
            id = "someWhereUndefined",
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "The provided grid data source 'someWhereUndefined' is not supported!"
        }

        "identify missing csv parameters" in {
          val gridDataSource = InputConfig.GridDatasource(
            None,
            id = "csv",
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkGridDataSource(gridDataSource)
          }.getMessage shouldBe "No grid data source csv parameters provided. If you intend to read grid data from " +
            ".csv-files, please provide .csv parameters!"
        }

        "let valid csv grid data source definition pass" in {
          val gridDataSource = InputConfig.GridDatasource(
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

      "Checking the primary data sources" should {
        val checkPrimaryDataSource =
          PrivateMethod[Unit](Symbol("checkPrimaryDataSource"))

        "lead to complaining about too much source definitions" in {
          val maliciousConfig = PrimaryConfig(
            Some(CouchbaseParams("", "", "", "", "", "", "")),
            Some(TimeStampedCsvParams("", "", isHierarchic = false, "")),
            None,
            None,
          )

          val exception = intercept[InvalidConfigParameterException](
            ConfigFailFast invokePrivate checkPrimaryDataSource(maliciousConfig)
          )
          exception.getMessage shouldBe "2 time series source types defined. Please define only one type!\nAvailable types:\n\tcsv\n\tsql"
        }

        "lead to complaining about too few source definitions" in {
          val maliciousConfig = PrimaryConfig(
            None,
            None,
            None,
            None,
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPrimaryDataSource(maliciousConfig)
          }
        }

        "not let couchbase parameters pass for mapping configuration" in {
          val maliciousConfig = PrimaryConfig(
            Some(CouchbaseParams("", "", "", "", "", "", "")),
            None,
            None,
            None,
          )

          val exception = intercept[InvalidConfigParameterException](
            ConfigFailFast invokePrivate checkPrimaryDataSource(maliciousConfig)
          )
          exception.getMessage shouldBe "Invalid configuration 'CouchbaseParams(,,,,,,)' for a time series source.\nAvailable types:\n\tcsv\n\tsql"
        }

        "let csv parameters pass for mapping configuration" in {
          val mappingConfig = PrimaryConfig(
            None,
            Some(TimeStampedCsvParams("", "", isHierarchic = false, "")),
            None,
            None,
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPrimaryDataSource(mappingConfig)
          }
        }

        "not let influx db parameters pass for mapping configuration" in {
          val maliciousConfig = PrimaryConfig(
            None,
            None,
            Some(TimeStampedInfluxDb1xParams("", 0, "", "")),
            None,
          )

          val exception = intercept[InvalidConfigParameterException](
            ConfigFailFast invokePrivate checkPrimaryDataSource(maliciousConfig)
          )
          exception.getMessage shouldBe "Invalid configuration 'TimeStampedInfluxDb1xParams(,0,,)' for a time series source.\nAvailable types:\n\tcsv\n\tsql"
        }

        "fails on invalid time pattern with csv" in {
          val invalidTimePatternConfig = PrimaryConfig(
            None,
            Some(TimeStampedCsvParams("", "", isHierarchic = false, "xYz")),
            None,
            None,
          )

          intercept[InvalidConfigParameterException](
            ConfigFailFast invokePrivate checkPrimaryDataSource(
              invalidTimePatternConfig
            )
          ).getMessage shouldBe "Invalid timePattern 'xYz' found. " +
            "Please provide a valid pattern!\nException: java.lang.IllegalArgumentException: Illegal pattern character 'x'"

        }

        "succeeds on valid time pattern with csv" in {
          val validTimePatternConfig = PrimaryConfig(
            None,
            Some(
              TimeStampedCsvParams(
                "",
                "",
                isHierarchic = false,
                "yyyy-MM-dd'T'HH:mm'Z[UTC]'",
              )
            ),
            None,
            None,
          )

          noException shouldBe thrownBy {
            ConfigFailFast invokePrivate checkPrimaryDataSource(
              validTimePatternConfig
            )
          }
        }
      }

      "Checking weather data sources" should {
        val checkWeatherDataSource =
          PrivateMethod[Unit](Symbol("checkWeatherDataSource"))

        val csv: BaseCsvParams =
          BaseCsvParams(",", "input", isHierarchic = false)
        val sample = SampleParams(true)

        val weatherDataSource = WeatherDatasource(
          CoordinateSource(
            None,
            "icon",
            Some(SampleParams(true)),
            None,
          ),
          None,
          None,
          None,
          50000d,
          360L,
          None,
          "icon",
          None,
          Some("yyyy-MM-dd HH:mm"),
        )

        "detects invalid weather data scheme" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkWeatherDataSource(
              weatherDataSource.copy(scheme = "this won't work")
            )
          }.getMessage shouldBe "The weather data scheme 'this won't work' is not supported. " +
            "Supported schemes:\n\ticon\n\tcosmo"
        }

        "detect missing source" in {
          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkWeatherDataSource(
              weatherDataSource
            )
          }.getMessage should startWith(
            "No weather source defined! This is currently not supported! Please provide the config parameters for " +
              "one of the following weather sources:"
          )
        }

        "detect too many sources" in {
          val tooManySources = weatherDataSource.copy(
            csvParams = Some(csv),
            sampleParams = Some(sample),
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkWeatherDataSource(tooManySources)
          }.getMessage should startWith("Multiple weather sources defined:")
        }

        "detects sample source mismatch" in {
          val csvCoordinateSource = new CoordinateSource(
            csvParams = Some(csv),
            gridModel = "icon",
            sampleParams = None,
            sqlParams = None,
          )

          val sampleMismatch = weatherDataSource.copy(
            coordinateSource = csvCoordinateSource,
            sampleParams = Some(sample),
          )

          intercept[InvalidConfigParameterException] {
            ConfigFailFast invokePrivate checkWeatherDataSource(sampleMismatch)
          }.getMessage shouldBe "Invalid coordinate source 'csv' defined for SampleWeatherSource. Please adapt the configuration to use sample coordinate source for weather data!"
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

        val defaultConfig: StorageRuntimeConfig =
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            -0.5,
            Some(0.8),
          )
        val storageConfig = AssetConfigs(defaultConfig, List.empty)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe "StorageRuntimeConfig: Default initial SOC needs to be between 0.0 and 1.0."
      }

      "throw exception if default target SOC is negative" in {
        val defaultConfig: StorageRuntimeConfig =
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(-0.8),
          )
        val storageConfig = AssetConfigs(defaultConfig, List.empty)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe "StorageRuntimeConfig: Default target SOC needs to be between 0.0 and 1.0."
      }

      "throw exception if individual initial SOC is negative" in {
        val uuid = java.util.UUID.randomUUID().toString
        val defaultConfig: StorageRuntimeConfig =
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        val individualConfig: List[StorageRuntimeConfig] = List(
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(uuid),
            -0.5,
            Some(0.8),
          )
        )
        val storageConfig = AssetConfigs(defaultConfig, individualConfig)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe s"StorageRuntimeConfig: List($uuid) initial SOC needs to be between 0.0 and 1.0."
      }

      "throw exception if individual target SOC is negative" in {
        val uuid = java.util.UUID.randomUUID().toString
        val defaultConfig: StorageRuntimeConfig =
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        val individualConfig: List[StorageRuntimeConfig] = List(
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(uuid),
            0.5,
            Some(-0.8),
          )
        )
        val storageConfig = AssetConfigs(defaultConfig, individualConfig)

        intercept[RuntimeException] {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }.getMessage shouldBe s"StorageRuntimeConfig: List($uuid) target SOC needs to be between 0.0 and 1.0."
      }

      "not throw exception if all parameters are in parameter range" in {
        val defaultConfig: StorageRuntimeConfig =
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        val individualConfig: List[StorageRuntimeConfig] = List(
          StorageRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            1.0,
            List(java.util.UUID.randomUUID().toString),
            0.5,
            Some(0.8),
          )
        )
        val storageConfig = AssetConfigs(defaultConfig, individualConfig)

        noException should be thrownBy {
          ConfigFailFast invokePrivate checkStorageConfigs(storageConfig)
        }
      }
    }

    "Checking coordinate sources" should {
      val checkCoordinateSource =
        PrivateMethod[Unit](Symbol("checkCoordinateSource"))
      val csvParams: BaseCsvParams = BaseCsvParams(
        ",",
        "input",
        isHierarchic = false,
      )
      val sampleParams = SampleParams(true)

      val coordinateSource = new CoordinateSource(
        csvParams = None,
        gridModel = "icon",
        sampleParams = None,
        sqlParams = None,
      )

      "detect missing source" in {
        intercept[InvalidConfigParameterException] {
          ConfigFailFast invokePrivate checkCoordinateSource(coordinateSource)
        }.getMessage should startWith(
          "No coordinate source defined! This is currently not supported! Please provide the config parameters for one of the following coordinate sources"
        )
      }

      "detect too many sources" in {
        val tooManySources = coordinateSource.copy(
          csvParams = Some(csvParams),
          sampleParams = Some(sampleParams),
        )

        intercept[InvalidConfigParameterException] {
          ConfigFailFast invokePrivate checkCoordinateSource(tooManySources)
        }.getMessage should startWith("Multiple coordinate sources defined:")
      }

      "detect invalid grid model" in {
        val invalidGridModel = coordinateSource.copy(
          csvParams = Some(csvParams),
          gridModel = "invalid",
        )

        intercept[InvalidConfigParameterException] {
          ConfigFailFast invokePrivate checkCoordinateSource(invalidGridModel)
        }.getMessage should startWith("Grid model 'invalid' is not supported!")
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
