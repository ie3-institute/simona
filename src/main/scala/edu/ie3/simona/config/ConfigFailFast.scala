/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{Config, ConfigException}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.config.IoConfigUtils.{InfluxDb1xParams, ResultKafkaParams}
import edu.ie3.simona.config.OutputConfig.BaseOutputConfig
import edu.ie3.simona.config.RuntimeConfig.{
  BaseRuntimeConfig,
  RuntimeParticipantConfig
}
import edu.ie3.simona.config.SimonaConfig.RefSystemConfig
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.weather.WeatherSource
import edu.ie3.simona.util.CollectionUtils
import edu.ie3.simona.util.ConfigUtil.DatabaseConfigUtil.{
  checkInfluxDb1xParams,
  checkKafkaParams
}
import edu.ie3.simona.util.ConfigUtil.{CsvConfigUtil, NotifierIdentifier}
import edu.ie3.util.scala.ReflectionTools
import edu.ie3.util.{StringUtils, TimeUtil}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.time.format.DateTimeParseException
import java.util.UUID
import scala.util.{Failure, Success, Try}

/** Sanity checks for [[SimonaConfig]] that should lead to a fast failure during
  * simulation to avoid unnecessary loading and initialization steps. This
  * mostly includes logic checks from the SIMONA logic point of view (e.g.
  * missing config parameters where at least one is needed or check for invalid
  * or contradicting parameters
  */
case object ConfigFailFast extends LazyLogging {

  def check(typeSafeConfig: Config, simonaConfig: SimonaConfig): Unit = {
    check(typeSafeConfig)
    check(simonaConfig)
  }

  /** Checking the [[Config]], that aside of SIMONA also holds akka
    * configuration
    *
    * @param typeSafeConfig
    *   Config to check
    */
  def check(typeSafeConfig: Config): Unit = {
    checkAkkaConfig(typeSafeConfig)
  }

  /** Trying to get akka config and checking it
    *
    * @param typeSafeConfig
    *   Config to check
    */
  private def checkAkkaConfig(typeSafeConfig: Config): Unit = {
    Try(typeSafeConfig.getConfig("akka"))
      .map(akkaConfig => checkAkkaLoggers(akkaConfig)) match {
      case Failure(_: ConfigException.Missing) =>
        logger.warn(
          "There is no akka config at all. Did you include akka config (properly)?"
        )
      case Failure(exception) =>
        throw new InvalidConfigParameterException(
          "Checking of akka config failed due to unhandled error.",
          exception
        )
      case Success(_) =>
    }
  }

  /** Try to check the akka logging config
    *
    * @param typeSafeConfig
    *   Config to check
    */
  private def checkAkkaLoggers(typeSafeConfig: Config): Unit = {
    Try(typeSafeConfig.getIsNull("loggers")) match {
      case Success(true) | Failure(_: ConfigException.Missing) =>
        logger.warn(
          "Akka loggers are not specified. Did you include akka config (properly)?"
        )
      case Failure(exception) =>
        throw new InvalidConfigParameterException(
          "Checking of akka logging config failed due to unhandled error.",
          exception
        )
      case _ =>
    }
  }

  /** Main entrance to check a provided [[SimonaConfig]]
    *
    * @param simonaConfig
    *   the config that should be checked
    */
  def check(simonaConfig: SimonaConfig): Unit = {

    /* check date and time */
    checkTimeConfig(simonaConfig.time)

    // check if the provided combinations of refSystems provided are valid
    val refSystems = simonaConfig.gridConfig.refSystems
    refSystems.foreach(checkRefSystem)

    /* Check all participant model configurations */
    checkParticipantRuntimeConfiguration(
      simonaConfig.runtime.participant
    )

    /* Check the runtime listener configuration */
    checkRuntimeListenerConfiguration(
      simonaConfig.runtime.listener
    )

    /* Check if the provided combination of data source and parameters are valid */
    checkGridDataSource(simonaConfig.input.grid.datasource)

    /* Check correct parameterization of primary source */
    checkPrimaryDataSource(simonaConfig.input.primary)

    /* Check if the provided combination of data source and parameters are valid */
    checkWeatherDataSource(simonaConfig.input.weather.datasource)

    /* check if at least one data sink is defined */
    checkDataSink(simonaConfig.output.sink)

    /* Check all output configurations for participant models */
    checkParticipantsOutputConfig(
      simonaConfig.output.participant
    )

    /* Check power flow resolution configuration */
    checkPowerFlowResolutionConfiguration(simonaConfig.powerflow)
  }

  /** Checks for valid sink configuration
    *
    * @param sink
    *   the sink configuration that should be checked
    */
  private def checkDataSink(sink: OutputConfig.OutputSinkConfig): Unit = {
    // ensures failure if new output sinks are added to enforce adaptions of the check sink method as well
    val supportedSinks = Set("influxdb1x", "csv", "kafka")
    if (
      !sink.productElementNames
        .map(_.trim.toLowerCase)
        .toSet
        .equals(supportedSinks)
    )
      throw new InvalidConfigParameterException(
        s"Newly added sink(s) " +
          s"'${sink.productElementNames.map(_.toLowerCase).toSet.removedAll(supportedSinks)}' detected! " +
          s"Please adapt 'ConfigFailFast' accordingly! Currently supported sinks: ${supportedSinks.mkString(", ")}."
      )

    // failure if all sinks are not-configured
    val sinkConfigs = ReflectionTools.classFieldToVal(sink).values.map {
      case o: Option[_] => o
      case _ =>
        throw new InvalidConfigParameterException(
          "All sinks in configuration must be optional!"
        )
    }

    if (sinkConfigs.forall(_.isEmpty))
      throw new InvalidConfigParameterException(
        "No sink configuration found! Please ensure that at least one sink is " +
          s"configured! You can choose from: ${supportedSinks.mkString(", ")}."
      )

    if (sinkConfigs.count(_.isDefined) > 1)
      throw new InvalidConfigParameterException(
        "Multiple sink configurations are not supported! Please ensure that only " +
          "one sink is configured!"
      )

    sinkConfigs.find(_.isDefined) match {
      case Some(Some(influxDb1x: InfluxDb1xParams)) =>
        checkInfluxDb1xParams(
          "Sink",
          ResultSinkType.buildInfluxDb1xUrl(influxDb1x),
          influxDb1x.database
        )
      case Some(Some(kafka: ResultKafkaParams)) =>
        checkKafkaParams(kafka, Seq(kafka.topicNodeRes))
      case _ => // do nothing
    }

  }

  /** Check time configuration
    *
    * @param timeConfig
    *   the time config
    */
  private def checkTimeConfig(
      timeConfig: SimonaConfig.TimeConfig
  ): Unit = {

    val startDate = createDateTime(timeConfig.startDateTime)
    val endDate = createDateTime(timeConfig.endDateTime)

    if (startDate.isAfter(endDate))
      throw new InvalidConfigParameterException(
        s"Invalid time configuration." +
          s"Please ensure that the start time of the simulation is before the end time."
      )
  }

  /** Creates a ZonedDateTime from String. If a faulty dateTime string is
    * passed, an [[InvalidConfigParameterException]] is thrown
    *
    * @param dateTimeString
    *   the dateTimeString that should be checked
    */
  private def createDateTime(
      dateTimeString: String
  ): ZonedDateTime = {
    try {
      TimeUtil.withDefaults.toZonedDateTime(dateTimeString)
    } catch {
      case e: DateTimeParseException =>
        throw new InvalidConfigParameterException(
          s"Invalid dateTimeString: $dateTimeString." +
            s"Please ensure that your date/time parameter match the following pattern: 'yyyy-MM-dd HH:mm:ss'",
          e
        )
    }
  }

  /** Checks all participant model runtime sub configuration trees
    *
    * @param subConfig
    *   Sub configuration tree to check
    */
  private def checkParticipantRuntimeConfiguration(
      subConfig: RuntimeConfig.RuntimeParticipantsConfig
  ): Unit = {
    if (subConfig.requestVoltageDeviationThreshold < 0)
      throw new InvalidConfigParameterException(
        "The participant power request voltage deviation threshold must be positive!"
      )

    subConfig.asSeq.foreach(checkBaseRuntimeConfigs(_))

    /* check model configuration parameters specific to participants */
    // load model
    (subConfig.load.defaultConfig +: subConfig.load.individualConfigs)
      .foreach(checkSpecificLoadModelConfig)
  }

  /** Check the runtime event listener config
    * @param listenerConfig
    *   the runtime listener config
    */
  private def checkRuntimeListenerConfiguration(
      listenerConfig: RuntimeConfig.RuntimeListenerConfig
  ): Unit = {
    listenerConfig.kafka.foreach(kafka =>
      checkKafkaParams(kafka, Seq(kafka.topic))
    )
  }

  /** Check participants's basic runtime configurations, as well as in default
    * as in individual configs. This comprises
    * i.e. uuid and scaling factor
    */
  private def checkBaseRuntimeConfigs[T <: BaseRuntimeConfig](
      cfg: RuntimeParticipantConfig[T],
      defaultString: String = "default"
  ): Unit = {
    // special default config check
    val uuidString = cfg.defaultConfig.uuids.mkString(",")
    if (
      StringUtils
        .cleanString(uuidString)
        .toLowerCase != StringUtils.cleanString(defaultString).toLowerCase
    )
      logger.warn(
        s"You provided '$uuidString' as uuid reference for the default model config. Those references will not be considered!"
      )

    // special individual configs check
    /* Check, if there are ambiguous configs and then check all configs */
    if (!CollectionUtils.isUniqueSeq(cfg.individualConfigs.flatMap(_.uuids)))
      throw new InvalidConfigParameterException(
        "The basic model configurations contain ambiguous definitions."
      )

    // check that is valid for all model configs
    val allConfigs = Map(cfg.defaultConfig -> Some(defaultString)) ++
      cfg.individualConfigs.map(config => (config, None)).toMap

    allConfigs.foreach { case (config, singleEntryStringOpt) =>
      /* Checking the uuids */
      if (config.uuids.isEmpty)
        throw new InvalidConfigParameterException(
          "There has to be at least one identifier for each participant."
        )
      /* If there is an option to a String that is also valid as a single entry, then check for this */
      singleEntryStringOpt match {
        case Some(singleString) =>
          checkSingleString(singleString, config.uuids)
        case None =>
          config.uuids.foreach(uuid =>
            try {
              UUID.fromString(uuid)
            } catch {
              case e: IllegalArgumentException =>
                throw new InvalidConfigParameterException(
                  s"The UUID '$uuid' cannot be parsed as it is invalid.",
                  e
                )
            }
          )
      }

      // check for scaling
      if (config.scaling < 0)
        throw new InvalidConfigParameterException(
          s"The scaling factor for system participants with UUID '${config.uuids.mkString(",")}' may not be negative."
        )
    }
  }

  /** Check method for a single string, normally the default string
    *
    * @param singleString
    *   the single string that is expected
    * @param uuids
    *   the corresponding list of uuids
    */
  private def checkSingleString(
      singleString: String,
      uuids: Seq[String]
  ): Unit = {
    if (uuids.toVector.size != 1)
      throw new InvalidConfigParameterException(
        "The list of UUIDs is supposed to only have one entry!"
      )
    uuids.headOption match {
      case Some(singleEntry) =>
        if (
          StringUtils
            .cleanString(singleEntry)
            .toLowerCase() != singleString
        )
          try {
            UUID.fromString(singleEntry)
          } catch {
            case e: IllegalArgumentException =>
              throw new InvalidConfigParameterException(
                s"Found invalid UUID '$singleEntry' it was meant to be the string '$singleString' or a valid UUID.",
                e
              )
          }
      case None =>
        throw new InvalidConfigParameterException(
          "There is no valid uuid entry in the list."
        )
    }
  }

  /** Check model configuration parameters specific to the load model, i.e.
    * model behaviour and reference
    */
  private def checkSpecificLoadModelConfig(
      loadModelConfig: RuntimeConfig.LoadRuntimeConfig
  ): Unit = {
    if (!LoadModelBehaviour.isEligibleInput(loadModelConfig.modelBehaviour))
      throw new InvalidConfigParameterException(
        s"The load model behaviour '${loadModelConfig.modelBehaviour}' for the loads with UUIDs '${loadModelConfig.uuids
            .mkString(",")}' is invalid."
      )

    if (
      !LoadReference.isEligibleKey(
        loadModelConfig.reference
      )
    )
      throw new InvalidConfigParameterException(
        s"The standard load profile reference '${loadModelConfig.reference}' for the loads with UUIDs '${loadModelConfig.uuids
            .mkString(",")}' is invalid."
      )
  }

  /** Sanity checks for a [[SimonaConfig.RefSystemConfig]]
    *
    * @param refSystem
    *   the [[SimonaConfig.RefSystemConfig]] that should be checked
    */
  private def checkRefSystem(refSystem: RefSystemConfig): Unit = {

    val voltLvls =
      refSystem.voltLvls.getOrElse(List.empty[SimonaConfig.VoltLvlConfig])
    val gridIds = refSystem.gridIds.getOrElse(List.empty[String])

    if (voltLvls.isEmpty && gridIds.isEmpty)
      throw new InvalidConfigParameterException(
        "The provided values for voltLvls and gridIds are empty! " +
          s"At least one of these optional parameters has to be provided for a valid refSystem! " +
          s"Provided refSystem is: $refSystem."
      )

    voltLvls.foreach { voltLvl =>
      Try(Quantities.getQuantity(voltLvl.vNom)) match {
        case Success(quantity) =>
          if (!quantity.getUnit.isCompatible(Units.VOLT))
            throw new InvalidConfigParameterException(
              s"The given nominal voltage '${voltLvl.vNom}' cannot be parsed to electrical potential! Please provide the volt level with its unit, e.g. \"20 kV\""
            )
        case Failure(exception) =>
          throw new InvalidConfigParameterException(
            s"The given nominal voltage '${voltLvl.vNom}' cannot be parsed to a quantity. Did you provide the volt level with it's unit (e.g. \"20 kV\")?",
            exception
          )
      }
    }

    gridIds.foreach {
      case gridIdRange @ ConfigConventions.gridIdDotRange(from, to) =>
        rangeCheck(from.toInt, to.toInt, gridIdRange)
      case gridIdRange @ ConfigConventions.gridIdMinusRange(from, to) =>
        rangeCheck(from.toInt, to.toInt, gridIdRange)
      case ConfigConventions.singleGridId(_) =>
      case gridId =>
        throw new InvalidConfigParameterException(
          s"The provided gridId $gridId is malformed!"
        )
    }

    refSystem.sNom match {
      case ConfigConventions.refSystemQuantRegex(_) =>
      case _ =>
        throw new InvalidConfigParameterException(
          s"Invalid value for sNom from provided refSystem $refSystem. Is a valid unit provided?"
        )
    }

    refSystem.vNom match {
      case ConfigConventions.refSystemQuantRegex(_) =>
      case _ =>
        throw new InvalidConfigParameterException(
          s"Invalid value for vNom from provided refSystem $refSystem. Is a valid unit provided?"
        )
    }

    def rangeCheck(from: Int, to: Int, gridIdRange: String): Unit = {
      if (from >= to)
        throw new InvalidConfigParameterException(
          s"Invalid gridId Range $gridIdRange. Start $from cannot be equals or bigger than end $to."
        )
    }
  }

  private def checkGridDataSource(
      gridDataSource: InputConfig.GridDataSource
  ): Unit = {

    // grid source information provided?
    if (gridDataSource.id.isEmpty) {
      throw new InvalidConfigParameterException(
        "No grid data source information provided! Cannot proceed!"
      )
    }

    // optional parameter check for different grid sources
    gridDataSource.id.toLowerCase match {
      case "csv" =>
        gridDataSource.csvParams match {
          case Some(csvParams) =>
            CsvConfigUtil.checkBaseCsvParams(csvParams, "GridSource")
          case None =>
            throw new InvalidConfigParameterException(
              "No grid data source csv parameters provided. If you intend to read grid data from .csv-files, please " +
                "provide .csv parameters!"
            )
        }

      case _ =>
        throw new InvalidConfigParameterException(
          s"The provided grid data source '${gridDataSource.id}' is not supported!"
        )
    }
  }

  private def checkPrimaryDataSource(
      primary: InputConfig.PrimaryConfig
  ): Unit =
    PrimaryServiceProxy.checkConfig(primary)

  private def checkWeatherDataSource(
      dataSourceConfig: InputConfig.WeatherDataSourceConfig
  ): Unit = WeatherSource.checkConfig(dataSourceConfig)

  /** Check the config sub tree for output parameterization
    *
    * @param subConfig
    *   Output sub config tree for participants
    */
  private def checkParticipantsOutputConfig(
      subConfig: OutputConfig.ParticipantOutputConfig
  ): Unit = {

    (subConfig.defaultConfig +: subConfig.individualConfigs).foreach(c =>
      if (c.powerRequestReply)
        throw new NotImplementedError(
          "PowerRequestReply output handling is not supported yet!"
        )
    )

    checkDefaultBaseOutputConfig(
      subConfig.defaultConfig,
      defaultString = "default"
    )
    checkIndividualParticipantsOutputConfigs(subConfig.individualConfigs)
  }

  /** Checks resolution of power flow calculation
    *
    * @param powerFlow
    *   the power flow configuration that should be checked
    */
  private def checkPowerFlowResolutionConfiguration(
      powerFlow: SimonaConfig.PowerFlowConfig
  ): Unit = {

    val nanos = powerFlow.resolution.toNanos
    if (nanos % 1e9 != 0) {
      throw new InvalidConfigParameterException(
        "Invalid time resolution. Please ensure, that the time resolution " +
          "for power flow calculation is at least rounded to a full second!"
      )
    }
  }

  /** Check the default config
    *
    * @param config
    *   Config to check
    * @param defaultString
    *   String that is meant to denote the default config
    */
  private def checkDefaultBaseOutputConfig(
      config: BaseOutputConfig,
      defaultString: String
  ): Unit = {
    if (
      StringUtils
        .cleanString(config.notifier)
        .toLowerCase != StringUtils.cleanString(defaultString).toLowerCase
    )
      logger.warn(
        s"You provided '${config.notifier}' as model type for the default participant output config. This will not be considered!"
      )
  }

  /** Checks the participant output configurations on duplicates
    *
    * @param configs
    *   List of individual config entries
    */
  private def checkIndividualParticipantsOutputConfigs(
      configs: Seq[BaseOutputConfig]
  ): Unit = {
    val duplicateKeys = configs
      .map(config => StringUtils.cleanString(config.notifier).toLowerCase())
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter { case (_, count) =>
        count > 1
      }
      .keys

    if (duplicateKeys.nonEmpty)
      throw new InvalidConfigParameterException(
        s"There are multiple output configurations for participant types '${duplicateKeys.mkString(",")}'."
      )

    configs.foreach(checkBaseOutputConfig)
  }

  /** Check the content of a [[BaseOutputConfig]]
    *
    * @param config
    *   to be checked
    */
  private def checkBaseOutputConfig(config: BaseOutputConfig): Unit = {
    checkNotifierIdentifier(config.notifier)
  }

  /** Check the validity of the identifier String
    *
    * @param id
    *   identifier String to check
    */
  private def checkNotifierIdentifier(id: String): Unit = {
    try {
      NotifierIdentifier(id)
    } catch {
      case e: NoSuchElementException =>
        throw new InvalidConfigParameterException(
          s"The identifier '$id' you provided is not valid. Valid input: ${NotifierIdentifier.values.map(_.toString).mkString(",")}",
          e
        )
    }
  }
}
