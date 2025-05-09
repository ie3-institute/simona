/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.connectors.SqlConnector
import edu.ie3.datamodel.io.csv.CsvLoadProfileMetaInformation
import edu.ie3.datamodel.io.factory.timeseries.{
  BdewLoadProfileFactory,
  LoadProfileFactory,
  RandomLoadProfileFactory,
}
import edu.ie3.datamodel.io.naming.timeseries.LoadProfileMetaInformation
import edu.ie3.datamodel.io.naming.{DatabaseNamingStrategy, FileNamingStrategy}
import edu.ie3.datamodel.io.source.csv.{
  CsvDataSource,
  CsvLoadProfileSource,
  CsvTimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.io.source.sql.{
  SqlDataSource,
  SqlLoadProfileSource,
  SqlTimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.io.source.{
  DataSource,
  LoadProfileSource,
  TimeSeriesMetaInformationSource,
}
import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.datamodel.models.timeseries.repetitive.LoadProfileTimeSeries
import edu.ie3.datamodel.models.value.load.{
  BdewLoadValues,
  LoadValues,
  RandomLoadValues,
}
import edu.ie3.simona.config.ConfigParams.{BaseCsvParams, BaseSqlParams}
import edu.ie3.simona.config.InputConfig
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.Path
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.{Failure, Success, Try}

/** Utility methods for loading csv and sql load profile sources.
  */
object LoadProfileSources {

  private val log: Logger = LoggerFactory.getLogger(LoadProfileSources.getClass)

  /** Initializes the load profile sources.
    *
    * @param sourceDefinition
    *   The definition of additional sources. If no definition is given, only
    *   the build in load profiles can be used.
    * @return
    *   The option for the build in [[LoadProfileTimeSeries]] as well as a map:
    *   load profile to source
    */
  def buildSources(
      sourceDefinition: InputConfig.LoadProfile.Datasource
  ): Map[LoadProfile, LoadProfileSource[_, _]] = {
    val definedSources = Vector(
      sourceDefinition.csvParams,
      sourceDefinition.sqlParams,
    ).find(_.isDefined).flatten

    // get the provided sources
    val sourceOptions = getSourceOptions(definedSources)

    // build the load profile sources
    sourceOptions match {
      case Some(
            (
              dataSource: DataSource,
              metaInformationSource: TimeSeriesMetaInformationSource,
            )
          ) =>
        // get the meta information
        val metaInformation: Map[String, LoadProfileMetaInformation] =
          metaInformationSource.getLoadProfileMetaInformation.asScala.toMap

        // build all defined sources
        val bdew = buildSourcesFrom(
          dataSource,
          metaInformation,
          new BdewLoadProfileFactory(),
          classOf[BdewLoadValues],
        )
        val random = buildSourcesFrom(
          dataSource,
          metaInformation,
          new RandomLoadProfileFactory(),
          classOf[RandomLoadValues],
        )
        val allSources = bdew ++ random

        // check if all sources are build
        checkSources(allSources.keySet, metaInformation.keySet)

        allSources
      case _ =>
        Map.empty
    }
  }

  /** Method for checking the provided options and returning an option for the
    * sources.
    * @param definedSources
    *   Option for source parameters.
    * @return
    *   An option for the [[DataSource]] as well as the
    *   [[TimeSeriesMetaInformationSource]].
    */
  private def getSourceOptions(
      definedSources: Option[Any]
  ): Option[(DataSource, TimeSeriesMetaInformationSource)] =
    definedSources match {
      case Some(BaseCsvParams(csvSep, directoryPath, _)) =>
        val csvDataSource =
          new CsvDataSource(
            csvSep,
            Path.of(directoryPath),
            new FileNamingStrategy(),
          )

        val metaInformationSource = new CsvTimeSeriesMetaInformationSource(
          csvDataSource
        )

        Some((csvDataSource, metaInformationSource))

      case Some(BaseSqlParams(jdbcUrl, password, schemaName, _, userName)) =>
        val sqlConnector = new SqlConnector(
          jdbcUrl,
          userName,
          password,
        )
        val namingStrategy = new DatabaseNamingStrategy()
        val sqlDataSource: SqlDataSource =
          new SqlDataSource(sqlConnector, schemaName, namingStrategy)

        val metaInformationSource = new SqlTimeSeriesMetaInformationSource(
          sqlConnector,
          schemaName,
          namingStrategy,
        )

        Some((sqlDataSource, metaInformationSource))

      case _ =>
        None
    }

  /** Method for checking the build sources.
    * @param profiles
    *   That have been built.
    * @param expectedProfiles
    *   That are expected.
    */
  private def checkSources(
      profiles: Set[LoadProfile],
      expectedProfiles: Set[String],
  ): Unit = {
    if (profiles.size != expectedProfiles.size) {
      expectedProfiles
        .diff(profiles.map(_.getKey))
        .foreach { profile =>
          log.warn(s"No factory found for profile $profile!")
        }
    }
  }

  /** Method to build [[LoadProfileSource]]s with a given
    * [[LoadProfileFactory]].
    * @param factory
    *   That is used to build.
    * @param entryClass
    *   Class of the entries that are excepted.
    * @param datasource
    *   For the [[LoadProfileSource]]
    * @param allMetaInformation
    *   All available meta information.
    * @tparam P
    *   Type of the [[LoadProfile]].
    * @tparam V
    *   Type of the [[LoadValues]].
    * @return
    *   A map: [[LoadProfile]] to [[LoadProfileSource]].
    */
  private def buildSourcesFrom[
      P <: LoadProfile,
      V <: LoadValues,
  ](
      datasource: DataSource,
      allMetaInformation: Map[String, LoadProfileMetaInformation],
      factory: LoadProfileFactory[P, V],
      entryClass: Class[V],
  ): Map[LoadProfile, LoadProfileSource[_, _]] = {
    val emptyMap = Map.empty[LoadProfile, LoadProfileSource[_, _]]

    // filter out all profile, that cannot be built by the given factory
    allMetaInformation
      .filter { case (profile, meta) =>
        Try(factory.parseProfile(profile)) match {
          case Failure(_) =>
            false
          case Success(value) =>
            value.getKey.equals(meta.getProfile)
        }
      }
      .foldLeft(emptyMap) { case (map, (profile, metaInformation)) =>
        val parsedProfile = factory.parseProfile(profile)

        (datasource, metaInformation) match {
          case (
                csvDataSource: CsvDataSource,
                csvLoadProfileMetaInformation: CsvLoadProfileMetaInformation,
              ) =>
            map ++ Map(
              parsedProfile -> new CsvLoadProfileSource[P, V](
                csvDataSource,
                csvLoadProfileMetaInformation,
                entryClass,
                factory,
              )
            )

          case (
                sqlDataSource: SqlDataSource,
                loadProfileMetaInformation: LoadProfileMetaInformation,
              ) =>
            map ++ Map(
              parsedProfile -> new SqlLoadProfileSource[P, V](
                sqlDataSource,
                loadProfileMetaInformation,
                entryClass,
                factory,
              )
            )
          case _ =>
            map
        }
      }
  }

}
