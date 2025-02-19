/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.{Config, ConfigException}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.config.RuntimeConfig.{
  BaseRuntimeConfig,
  LoadRuntimeConfig,
  StorageRuntimeConfig,
}
import edu.ie3.simona.config.ConfigParams._
import edu.ie3.simona.config.SimonaConfig._
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.weather.WeatherSource.WeatherScheme
import edu.ie3.simona.util.CollectionUtils
import edu.ie3.simona.util.ConfigUtil.CsvConfigUtil.checkBaseCsvParams
import edu.ie3.simona.util.ConfigUtil.DatabaseConfigUtil.{
  checkCouchbaseParams,
  checkInfluxDb1xParams,
  checkKafkaParams,
  checkSqlParams,
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
object ConfigFailFast extends LazyLogging {

  def check(typeSafeConfig: Config, simonaConfig: SimonaConfig): Unit = {
    check(typeSafeConfig)
    check(simonaConfig)
  }

  /** Checking the [[Config]], that aside of SIMONA also holds pekko
    * configuration
    *
    * @param typeSafeConfig
    *   Config to check
    */
  def check(typeSafeConfig: Config): Unit = {
    checkPekkoConfig(typeSafeConfig)
  }

  /** Trying to get pekko config and checking it
    *
    * @param typeSafeConfig
    *   Config to check
    */
  private def checkPekkoConfig(typeSafeConfig: Config): Unit = {
    Try(typeSafeConfig.getConfig("pekko"))
      .map(pekkoConfig => checkPekkoLoggers(pekkoConfig)) match {
      case Failure(_: ConfigException.Missing) =>
        logger.warn(
          "There is no pekko config at all. Did you include pekko config (properly)?"
        )
      case Failure(exception) =>
        throw new InvalidConfigParameterException(
          "Checking of pekko config failed due to unhandled error.",
          exception,
        )
      case Success(_) =>
    }
  }

  /** Try to check the pekko logging config
    *
    * @param typeSafeConfig
    *   Config to check
    */
  private def checkPekkoLoggers(typeSafeConfig: Config): Unit = {
    Try(typeSafeConfig.getIsNull("loggers")) match {
      case Success(true) | Failure(_: ConfigException.Missing) =>
        logger.warn(
          "Pekko loggers are not specified. Did you include pekko config (properly)?"
        )
      case Failure(exception) =>
        throw new InvalidConfigParameterException(
          "Checking of pekko logging config failed due to unhandled error.",
          exception,
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
    checkTimeConfig(simonaConfig.simona.time)

    // check if the provided combinations of refSystems provided are valid
    simonaConfig.simona.gridConfig.refSystems.foreach(checkRefSystem)

    // check if the provided combinations of voltageLimits provided are valid
    simonaConfig.simona.gridConfig.voltageLimits.foreach(checkVoltageLimits)

    /* Check all participant model configurations */
    checkParticipantRuntimeConfiguration(
      simonaConfig.simona.runtime.participant
    )

    /* Check the runtime listener configuration */
    checkRuntimeListenerConfiguration(
      simonaConfig.simona.runtime.listener
    )

    /* Check if the provided combination of data source and parameters are valid */
    checkGridDataSource(simonaConfig.simona.input.grid.datasource)

    /* Check correct parameterization of primary source */
    checkPrimaryDataSource(simonaConfig.simona.input.primary)

    /* Check if the provided combination of data source and parameters are valid */
    checkWeatherDataSource(simonaConfig.simona.input.weather.datasource)

    checkOutputConfig(simonaConfig.simona.output)

    /* Check power flow resolution configuration */
    checkPowerFlowResolutionConfiguration(simonaConfig.simona.powerflow)

    /* Check control scheme definitions */
    simonaConfig.simona.control.foreach(checkControlSchemes)

    /* Check correct parameterization of storages */
    checkStoragesConfig(simonaConfig.simona.runtime.participant.storage)
  }

  /** Checks for valid output configuration
    *
    * @param outputConfig
    *   the output configuration that should be checked
    */
  private def checkOutputConfig(
      outputConfig: OutputConfig
  ): Unit = {

    /* check if at least one data sink is defined */
    checkDataSink(outputConfig.sink)

    /* Check all output configurations for participant models */
    checkParticipantsOutputConfig(outputConfig.participant)

    /* Check all output configurations for thermal models */
    checkThermalOutputConfig(outputConfig.thermal)

    /* Check output configurations for log */
    checkLogOutputConfig(outputConfig.log)
  }

  /** Checks for valid sink configuration
    *
    * @param sink
    *   the sink configuration that should be checked
    */
  private def checkDataSink(sink: OutputConfig.Sink): Unit = {
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
          influxDb1x.database,
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
      timeConfig: SimonaConfig.Simona.Time
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
            s"Please ensure that your date/time parameter match the following pattern: 'yyyy-MM-dd'T'HH:mm:ss'Z''",
          e,
        )
    }
  }

  /** Checks all participant model runtime sub configuration trees
    *
    * @param subConfig
    *   Sub configuration tree to check
    */
  private def checkParticipantRuntimeConfiguration(
      subConfig: RuntimeConfig.Participant
  ): Unit = {
    if (subConfig.requestVoltageDeviationThreshold < 0)
      throw new InvalidConfigParameterException(
        "The participant power request voltage deviation threshold must be positive!"
      )

    /* Check basic model configuration parameters common to each participant */
    checkBaseRuntimeConfigs(
      subConfig.load.defaultConfig,
      subConfig.load.individualConfigs,
    )

    checkBaseRuntimeConfigs(
      subConfig.fixedFeedIn.defaultConfig,
      subConfig.fixedFeedIn.individualConfigs,
    )

    checkBaseRuntimeConfigs(
      subConfig.evcs.defaultConfig,
      subConfig.evcs.individualConfigs,
    )

    checkBaseRuntimeConfigs(
      subConfig.pv.defaultConfig,
      subConfig.pv.individualConfigs,
    )

    checkBaseRuntimeConfigs(
      subConfig.wec.defaultConfig,
      subConfig.wec.individualConfigs,
    )

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
      listenerConfig: RuntimeConfig.Listener
  ): Unit = {
    listenerConfig.kafka.foreach(kafka =>
      checkKafkaParams(kafka, Seq(kafka.topic))
    )
  }

  /** Check participants' basic runtime configurations, as well as in default as
    * in individual configs. This comprises
    * i.e. uuid and scaling factor
    */
  private def checkBaseRuntimeConfigs(
      defaultConfig: BaseRuntimeConfig,
      individualConfigs: List[BaseRuntimeConfig],
      defaultString: String = "default",
  ): Unit = {
    // special default config check
    val uuidString = defaultConfig.uuids.mkString(",")
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
    if (!CollectionUtils.isUniqueList(individualConfigs.flatMap(_.uuids)))
      throw new InvalidConfigParameterException(
        "The basic model configurations contain ambiguous definitions."
      )

    // check that is valid for all model configs
    val allConfigs = Map(defaultConfig -> Some(defaultString)) ++
      individualConfigs.map(config => (config, None)).toMap

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
                  e,
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
      uuids: List[String],
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
                e,
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
      loadModelConfig: LoadRuntimeConfig
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
    * @param refSystems
    *   a list of [[SimonaConfig.RefSystemConfig]]s that should be checked
    */
  private def checkRefSystem(
      refSystems: List[RefSystemConfig]
  ): Unit = {
    refSystems.foreach { refSystem =>
      checkGridConfig(refSystem, "refSystem")

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
    }
  }

  /** Sanity checks for a [[SimonaConfig.VoltageLimitsConfig]]
    *
    * @param voltageLimits
    *   the [[SimonaConfig.VoltageLimitsConfig]] that should be checked
    */
  private def checkVoltageLimits(
      voltageLimits: List[VoltageLimitsConfig]
  ): Unit = {
    voltageLimits.foreach { limit =>
      checkGridConfig(limit, "voltage limit")

      if (limit.vMin >= limit.vMax) {
        throw new InvalidConfigParameterException(
          s"Invalid value for vMin and vMax from provided voltage limit $limit. Is vMin smaller than vMax?"
        )
      }
    }
  }

  /** Method to check the common elements of a
    * [[SimonaConfig.Simona.GridConfig]].
    * @param gridConfig
    *   the individual config
    * @param configType
    *   the type of config (e.g. refSystem)
    */
  private def checkGridConfig(
      gridConfig: GridConfigParams,
      configType: String,
  ): Unit = {
    val voltLvls = gridConfig.voltLvls.getOrElse(List.empty)
    val gridIds = gridConfig.gridIds.getOrElse(List.empty)

    if (voltLvls.isEmpty && gridIds.isEmpty)
      throw new InvalidConfigParameterException(
        "The provided values for voltLvls and gridIds are empty! " +
          s"At least one of these optional parameters has to be provided for a valid $configType! " +
          s"Provided $configType is: $gridConfig."
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
            exception,
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

    def rangeCheck(from: Int, to: Int, gridIdRange: String): Unit = {
      if (from >= to)
        throw new InvalidConfigParameterException(
          s"Invalid gridId Range $gridIdRange. Start $from cannot be equals or bigger than end $to."
        )
    }
  }

  private def checkGridDataSource(
      gridDataSource: InputConfig.GridDatasource
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
      primary: InputConfig.Primary
  ): Unit =
    PrimaryServiceProxy.checkConfig(primary)

  private def checkWeatherDataSource(
      weatherDataSourceCfg: InputConfig.WeatherDatasource
  ): Unit = {
    // check coordinate source
    val definedCoordinateSource: String = checkCoordinateSource(
      weatherDataSourceCfg.coordinateSource
    )

    /* Check, if the column scheme is supported */
    if (!WeatherScheme.isEligibleInput(weatherDataSourceCfg.scheme))
      throw new InvalidConfigParameterException(
        s"The weather data scheme '${weatherDataSourceCfg.scheme}' is not supported. Supported schemes:\n\t${WeatherScheme.values
            .mkString("\n\t")}"
      )

    // check weather source parameters
    val supportedWeatherSources =
      Set("influxdb1x", "csv", "sql", "couchbase", "sample")
    val definedWeatherSources = Vector(
      weatherDataSourceCfg.sampleParams,
      weatherDataSourceCfg.csvParams,
      weatherDataSourceCfg.influxDb1xParams,
      weatherDataSourceCfg.couchbaseParams,
      weatherDataSourceCfg.sqlParams,
    ).filter(_.isDefined)

    // check that only one source is defined
    if (definedWeatherSources.size > 1)
      throw new InvalidConfigParameterException(
        s"Multiple weather sources defined: '${definedWeatherSources.map(_.getClass.getSimpleName).mkString("\n\t")}'." +
          s"Please define only one source!\nAvailable sources:\n\t${supportedWeatherSources.mkString("\n\t")}"
      )

    definedWeatherSources.headOption.flatten match {
      case Some(baseCsvParams: BaseCsvParams) =>
        checkBaseCsvParams(baseCsvParams, "WeatherSource")
      case Some(params: CouchbaseParams) =>
        checkCouchbaseParams(params)
      case Some(BaseInfluxDb1xParams(database, _, url)) =>
        checkInfluxDb1xParams("WeatherSource", url, database)
      case Some(params: SqlParams) =>
        checkSqlParams(params)
      case Some(_: SampleParams) =>
        // sample weather, no check required
        // coordinate source must be sample coordinate source
        if (weatherDataSourceCfg.coordinateSource.sampleParams.isEmpty) {
          // cannot use sample weather source with other combination of weather source than sample weather source
          throw new InvalidConfigParameterException(
            s"Invalid coordinate source " +
              s"'$definedCoordinateSource' defined for SampleWeatherSource. " +
              "Please adapt the configuration to use sample coordinate source for weather data!"
          )
        }
      case None | Some(_) =>
        throw new InvalidConfigParameterException(
          s"No weather source defined! This is currently not supported! Please provide the config parameters for one " +
            s"of the following weather sources:\n\t${supportedWeatherSources.mkString("\n\t")}"
        )
    }
  }

  /** Check the provided coordinate id data source configuration to ensure its
    * validity. For any invalid configuration parameters exceptions are thrown.
    *
    * @param coordinateSourceConfig
    *   the config to be checked
    * @return
    *   the name of the defined
    *   [[edu.ie3.datamodel.io.source.IdCoordinateSource]]
    */
  private def checkCoordinateSource(
      coordinateSourceConfig: InputConfig.CoordinateSource
  ): String = {
    val supportedCoordinateSources = Set("csv", "sql", "sample")
    val definedCoordSources = Vector(
      coordinateSourceConfig.sampleParams,
      coordinateSourceConfig.csvParams,
      coordinateSourceConfig.sqlParams,
    ).filter(_.isDefined)

    // check that only one source is defined
    if (definedCoordSources.size > 1)
      throw new InvalidConfigParameterException(
        s"Multiple coordinate sources defined: '${definedCoordSources.map(_.getClass.getSimpleName).mkString("\n\t")}'." +
          s"Please define only one source!\nAvailable sources:\n\t${supportedCoordinateSources.mkString("\n\t")}"
      )

    definedCoordSources.headOption.flatten match {
      case Some(baseCsvParams: BaseCsvParams) =>
        checkBaseCsvParams(baseCsvParams, "CoordinateSource")

        // check the grid model configuration
        val gridModel = coordinateSourceConfig.gridModel.toLowerCase
        if (gridModel != "icon" && gridModel != "cosmo") {
          throw new InvalidConfigParameterException(
            s"Grid model '$gridModel' is not supported!"
          )
        }

        "csv"
      case Some(sqlParams: SqlParams) =>
        checkSqlParams(sqlParams)
        "sql"
      case Some(_: SampleParams) =>
        "sample"
      case None | Some(_) =>
        throw new InvalidConfigParameterException(
          s"No coordinate source defined! This is currently not supported! Please provide the config parameters for one " +
            s"of the following coordinate sources:\n\t${supportedCoordinateSources.mkString("\n\t")}"
        )
    }

  }

  /** Check the config subtree for output parameterization
    *
    * @param subConfig
    *   Output sub config tree for participants
    */
  private def checkParticipantsOutputConfig(
      subConfig: AssetConfigs[OutputConfig.ParticipantOutputConfig]
  ): Unit = {

    (subConfig.defaultConfig :: subConfig.individualConfigs).foreach(c =>
      if (c.powerRequestReply)
        throw new NotImplementedError(
          "PowerRequestReply output handling is not supported yet!"
        )
    )

    implicit val elementType: String = "participant"

    checkDefaultBaseOutputConfig(subConfig.defaultConfig)
    checkIndividualOutputConfigs(subConfig.individualConfigs)
  }

  /** Check the config subtree for output parameterization
    *
    * @param subConfig
    *   Output sub config tree for participants
    */
  private def checkThermalOutputConfig(
      subConfig: AssetConfigs[OutputConfig.SimpleOutputConfig]
  ): Unit = {
    implicit val elementType: String = "thermal"
    checkDefaultBaseOutputConfig(subConfig.defaultConfig)
    checkIndividualOutputConfigs(subConfig.individualConfigs)
  }

  /** Check the config subtree for log output parameterization
    *
    * @param subConfig
    *   Output sub config tree for log
    */
  private def checkLogOutputConfig(
      subConfig: OutputConfig.Log
  ): Unit = {
    val validLogLevels = Seq("TRACE", "DEBUG", "INFO", "WARN", "ERROR")
    if (!validLogLevels.contains(subConfig.level))
      throw new InvalidConfigParameterException(
        s"Invalid log level \"${subConfig.level}\". Valid log levels: ${validLogLevels.mkString(", ")}"
      )
  }

  /** Checks resolution of power flow calculation
    *
    * @param powerFlow
    *   the power flow configuration that should be checked
    */
  private def checkPowerFlowResolutionConfiguration(
      powerFlow: SimonaConfig.Simona.Powerflow
  ): Unit = {

    // check if time bin is not smaller than in seconds
    val hasNanos = (powerFlow.resolution.toNanos / 1e9) % 1 != 0
    val hasMicros = (powerFlow.resolution.toMicros / 1e6) % 1 != 0
    val hasMillis = (powerFlow.resolution.toMillis / 1e3) % 1 != 0

    if (hasNanos || hasMicros || hasMillis) {
      throw new InvalidConfigParameterException(
        s"Invalid time resolution. Please ensure, that " +
          s"the time resolution for power flow calculation is at least rounded to a full second!"
      )
    }
  }

  /** Check the validity of control scheme definitions
    *
    * @param control
    *   Control scheme definitions
    */
  private def checkControlSchemes(control: Simona.Control): Unit = {
    control.transformer.foreach(checkTransformerControl)
  }

  /** Check the suitability of transformer control group definition.
    *
    * One important check cannot be performed at this place, as input data is
    * not available, yet: Do the measurements belong to a region, that can be
    * influenced by the transformer? This is partly addressed in
    * [[edu.ie3.simona.agent.grid.GridAgentFailFast]]
    *
    * @param transformerControlGroup
    *   Transformer control group definition
    */
  private def checkTransformerControl(
      transformerControlGroup: TransformerControlGroup
  ): Unit = {
    val lowerBoundary = 0.8
    val upperBoundary = 1.2
    transformerControlGroup match {
      case TransformerControlGroup(measurements, transformers, vMax, vMin) =>
        if (measurements.isEmpty)
          throw new InvalidConfigParameterException(
            s"A transformer control group (${transformerControlGroup.toString}) cannot have no measurements assigned."
          )
        if (transformers.isEmpty)
          throw new InvalidConfigParameterException(
            s"A transformer control group (${transformerControlGroup.toString}) cannot have no transformers assigned."
          )
        if (vMin < 0)
          throw new InvalidConfigParameterException(
            "The minimum permissible voltage magnitude of a transformer control group has to be positive."
          )
        if (vMax < vMin)
          throw new InvalidConfigParameterException(
            s"The minimum permissible voltage magnitude of a transformer control group (${transformerControlGroup.toString}) must be smaller than the maximum permissible voltage magnitude."
          )
        if (vMin < lowerBoundary)
          throw new InvalidConfigParameterException(
            s"A control group (${transformerControlGroup.toString}) which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
              s"by invalid parametrization of one control groups where vMin is lower than the lower boundary (0.8 of nominal Voltage)!"
          )
        if (vMax > upperBoundary)
          throw new InvalidConfigParameterException(
            s"A control group (${transformerControlGroup.toString}) which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
              s"by invalid parametrization of one control groups where vMax is higher than the upper boundary (1.2 of nominal Voltage)!"
          )
    }
  }

  /** Check the suitability of storage config parameters.
    *
    * @param storageRuntimeConfig
    *   RuntimeConfig of Storages
    */
  private def checkStoragesConfig(
      storageRuntimeConfig: AssetConfigs[StorageRuntimeConfig]
  ): Unit = {
    if (
      storageRuntimeConfig.defaultConfig.initialSoc < 0.0 || storageRuntimeConfig.defaultConfig.initialSoc > 1.0
    )
      throw new RuntimeException(
        s"StorageRuntimeConfig: Default initial SOC needs to be between 0.0 and 1.0."
      )

    if (
      storageRuntimeConfig.defaultConfig.targetSoc.exists(
        _ < 0.0
      ) || storageRuntimeConfig.defaultConfig.targetSoc.exists(_ > 1.0)
    )
      throw new RuntimeException(
        s"StorageRuntimeConfig: Default target SOC needs to be between 0.0 and 1.0."
      )

    storageRuntimeConfig.individualConfigs.foreach { config =>
      if (config.initialSoc < 0.0 || config.initialSoc > 1.0)
        throw new RuntimeException(
          s"StorageRuntimeConfig: ${config.uuids} initial SOC needs to be between 0.0 and 1.0."
        )

      if (config.targetSoc.exists(_ < 0.0) || config.targetSoc.exists(_ > 1.0))
        throw new RuntimeException(
          s"StorageRuntimeConfig: ${config.uuids} target SOC needs to be between 0.0 and 1.0."
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
      config: OutputConfig.BaseOutputConfig,
      defaultString: String = "default",
  )(implicit elementType: String): Unit = {
    if (
      StringUtils
        .cleanString(config.notifier)
        .toLowerCase != StringUtils.cleanString(defaultString).toLowerCase
    )
      logger.warn(
        s"You provided '${config.notifier}' as model type for the default $elementType output config. This will not be considered!"
      )
  }

  /** Checks the given output configurations on duplicates
    *
    * @param configs
    *   List of individual config entries
    */
  private def checkIndividualOutputConfigs(
      configs: List[OutputConfig.BaseOutputConfig]
  )(implicit elementType: String): Unit = {
    val duplicateKeys = configs
      .map(config => StringUtils.cleanString(config.notifier).toLowerCase())
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter { case (_, count) =>
        count > 1
      }
      .keys

    if (duplicateKeys.nonEmpty)
      throw new InvalidConfigParameterException(
        s"There are multiple output configurations for $elementType types '${duplicateKeys.mkString(",")}'."
      )

    implicit val exceptedNotifiers: Set[NotifierIdentifier.Value] =
      elementType match {
        case "participant" =>
          NotifierIdentifier.getParticipantIdentifiers
        case "thermal" =>
          NotifierIdentifier.getThermalIdentifiers
        case other =>
          throw new InvalidConfigParameterException(
            s"The output config for $other has no notifiers!"
          )
      }

    configs.foreach(checkBaseOutputConfig)
  }

  /** Check the content of a [[OutputConfig.BaseOutputConfig]]
    *
    * @param config
    *   to be checked
    * @param exceptedNotifiers
    *   a set of all valid identifiers
    */
  private def checkBaseOutputConfig(
      config: OutputConfig.BaseOutputConfig
  )(implicit exceptedNotifiers: Set[NotifierIdentifier.Value]): Unit = {
    checkNotifierIdentifier(config.notifier, exceptedNotifiers)
  }

  /** Check the validity of the identifier String
    *
    * @param id
    *   identifier String to check
    * @param exceptedNotifiers
    *   a set of all valid identifiers
    */
  private def checkNotifierIdentifier(
      id: String,
      exceptedNotifiers: Set[NotifierIdentifier.Value],
  ): Unit = {
    try {
      val notifier = NotifierIdentifier(id)

      if (!exceptedNotifiers.contains(notifier)) {
        throw new InvalidConfigParameterException(
          s"The identifier '$id' you provided is not valid. Valid input: ${exceptedNotifiers.map(_.toString).mkString(",")}"
        )
      }

    } catch {
      case e: NoSuchElementException =>
        throw new InvalidConfigParameterException(
          s"The identifier '$id' you provided is not valid. Valid input: ${exceptedNotifiers.map(_.toString).mkString(",")}",
          e,
        )
    }
  }
}
