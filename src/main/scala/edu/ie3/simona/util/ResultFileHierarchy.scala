/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.naming.{
  EntityPersistenceNamingStrategy,
  FileNamingStrategy,
}
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.FileHierarchyException
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.io.result.ResultSinkType.Csv
import edu.ie3.simona.logging.logback.LogbackConfiguration
import edu.ie3.util.io.FileIOUtils
import org.apache.commons.io.FilenameUtils.*

import scala.jdk.OptionConverters.RichOptional

/** Represents the output directory where the results will be materialized.
  */
final case class ResultFileHierarchy private (
    runOutputDir: Path,
    rawOutputDataFilePaths: Map[Class[_ <: ResultEntity], Path],
    configOutputDir: Path,
    logOutputDir: Path,
    tmpDir: Path,
    resultSinkType: ResultSinkType,
    resultEntitiesToConsider: Set[Class[_ <: ResultEntity]],
)

object ResultFileHierarchy extends LazyLogging {

  /** Creates the [[ResultFileHierarchy]] and relevant directories
    */
  def apply(
      outputDir: String,
      simulationName: String,
      resultEntityPathConfig: ResultEntityPathConfig,
      configureLogger: Path => Unit = LogbackConfiguration.default("INFO"),
      config: Option[(SimonaConfig, Config)] = None,
      addTimeStampToOutputDir: Boolean = true,
  ): ResultFileHierarchy = {

    val runStartTimeUTC = Option.when(addTimeStampToOutputDir)(
      new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(new java.util.Date())
    )

    val baseOutputDir = buildBaseOutputDir(outputDir)

    val runOutputDir = buildRunOutputDir(
      baseOutputDir,
      simulationName,
      runStartTimeUTC,
    )

    val configOutputDir = runOutputDir.resolve("configs")
    val rawOutputDataDir = runOutputDir.resolve("rawOutputData")
    val logOutputDir = runOutputDir.resolve("log")
    val tmpDir = runOutputDir.resolve("tmp")

    val resultSinkType: ResultSinkType = resultEntityPathConfig.resultSinkType

    val rawOutputDataFilePaths: Map[Class[_ <: ResultEntity], Path] = {
      resultSinkType match {
        case csv: Csv =>
          resultEntityPathConfig.resultEntitiesToConsider
            .map(resultEntityClass =>
              (
                resultEntityClass,
                ResultFileHierarchy.buildRawOutputFilePath(
                  resultEntityClass,
                  csv,
                  rawOutputDataDir,
                ),
              )
            )
            .toMap
        case _ =>
          Map.empty
      }
    }

    val dirsToBeCreated = Seq(
      baseOutputDir,
      runOutputDir,
      configOutputDir,
      rawOutputDataDir,
      logOutputDir,
      tmpDir,
    )

    val resultFileHierarchy = ResultFileHierarchy(
      runOutputDir,
      rawOutputDataFilePaths,
      configOutputDir,
      logOutputDir,
      tmpDir,
      resultSinkType,
      resultEntityPathConfig.resultEntitiesToConsider,
    )
    prepareDirectories(
      baseOutputDir,
      dirsToBeCreated,
      resultFileHierarchy,
      config,
    )

    // needs to be done after dir creation
    configureLogger(logOutputDir)

    resultFileHierarchy
  }

  /** Builds the base output directory
    *
    * @return
    *   the filepath of the directory
    */
  private def buildBaseOutputDir(
      outputDir: String
  ): Path = {
    // clean file string if necessary
    val cleanedBaseOutputDir = {
      val normalizedOutputDir = normalize(outputDir)
      (File.separator + "$").r.replaceAllIn(normalizedOutputDir, "")
    }

    Paths.get(cleanedBaseOutputDir)
  }

  /** Builds the output directory for this specific run
    */
  private def buildRunOutputDir(
      baseOutputDir: Path,
      simulationName: String,
      runStartTimeUTC: Option[String],
  ): Path = {
    val optionalSuffix =
      runStartTimeUTC.map(pattern => s"_$pattern").getOrElse("")

    baseOutputDir.resolve(s"$simulationName$optionalSuffix")
  }

  /** @param resultEntitiesToConsider
    *   [[ResultEntity]] s to consider to be written out
    * @param resultSinkType
    *   the type of the sink where result entities should be persisted
    */
  final case class ResultEntityPathConfig(
      resultEntitiesToConsider: Set[Class[_ <: ResultEntity]],
      resultSinkType: ResultSinkType,
  )

  /** @param modelClass
    *   the model class a file path should be built for
    * @param csvSink
    *   the csv sink type parameters
    * @param rawOutputDataDir
    *   the directory of the raw output data
    * @return
    *   an absolute file path as string for the provided model class incl. file
    *   name + extension
    */
  private def buildRawOutputFilePath(
      modelClass: Class[_ <: ResultEntity],
      csvSink: Csv,
      rawOutputDataDir: Path,
  ): Path = {
    val fileEnding =
      if (csvSink.fileFormat.startsWith("."))
        csvSink.fileFormat
      else ".".concat(csvSink.fileFormat)
    val namingStrategy = new FileNamingStrategy(
      new EntityPersistenceNamingStrategy(
        csvSink.filePrefix,
        csvSink.fileSuffix,
      )
    )
    val filename =
      namingStrategy.getFilePath(modelClass).toScala match {
        case Some(fileName) => fileName
        case None =>
          throw new FileHierarchyException(
            "Cannot get filename for " + modelClass.getSimpleName + ".class from PowerSystemDataModel file naming strategy!"
          )
      }

    rawOutputDataDir.resolve(s"${filename.toString}$fileEnding")
  }

  /** Prepares the output directories to be ready to hold the output data. This
    * includes creating the run directory with all subsequent directories as
    * well as copying the simulation configuration to the output dir
    *
    * @param baseOutputDir
    *   The base output directory
    * @param dirsToBeCreated
    *   The directories that need to be created
    * @param maybeConfig
    *   the config of the current simulation
    * @param resultFileHierarchy
    *   the output file hierarchy of the current simulation
    */
  private def prepareDirectories(
      baseOutputDir: Path,
      dirsToBeCreated: Seq[Path],
      resultFileHierarchy: ResultFileHierarchy,
      maybeConfig: Option[(SimonaConfig, Config)],
  ): Unit = {
    // create output directories if they are not present yet
    if (!runOutputDirExists(resultFileHierarchy))
      createOutputDirectories(
        baseOutputDir,
        dirsToBeCreated,
        resultFileHierarchy,
      )

    maybeConfig.foreach { case (simonaConfig, config) =>
      logger.info(
        "Processing configs for simulation: {}.",
        simonaConfig.simona.simulationName,
      )

      val outFile =
        resultFileHierarchy.configOutputDir.resolve("vn_simona.conf").toFile
      val bw = new BufferedWriter(new FileWriter(outFile))
      bw.write(
        config
          .root()
          .render(
            ConfigRenderOptions
              .defaults()
              .setOriginComments(false)
              .setComments(false)
          )
      )
      bw.close()
      logger.info("Config '{}' written to '{}'.", outFile.getPath, outFile)
    }

  }

  /** Checks if the directory of the current run already exists
    *
    * @param fileHierarchy
    *   the [[ResultFileHierarchy]] that holds information on the run directory
    *   path
    * @return
    *   true if it exists, false if not
    */
  def runOutputDirExists(fileHierarchy: ResultFileHierarchy): Boolean = {
    val outputDir = fileHierarchy.runOutputDir.toFile
    outputDir.exists() && outputDir.listFiles().length > 0
  }

  /** Creates all output directories of the provided [[ResultFileHierarchy]]
    *
    * @param outputFileHierarchy
    *   the [[ResultFileHierarchy]] the directories should be created for
    */
  private def createOutputDirectories(
      baseOutputDir: Path,
      dirsToBeCreated: Seq[Path],
      outputFileHierarchy: ResultFileHierarchy,
  ): Unit = {
    // try to create base output dir
    // / check for existence of the provided baseOutputDir, if not create it
    if (Files.exists(baseOutputDir) && baseOutputDir.toFile.isFile) {
      throw new FileHierarchyException(
        s"Provided base output path $baseOutputDir is a file and cannot be replaced with a directory!"
      )
    }

    // check if there is data inside the runOutputDir taking into account the provided FileHandling
    val runOutputDir = outputFileHierarchy.runOutputDir.toFile
    if (runOutputDir.exists() && runOutputDir.listFiles().length > 0) {
      // files inside the runOutputDir -> fail
      throw new FileHierarchyException(
        s"The runOutputDir ${outputFileHierarchy.runOutputDir.toString} already exists and is NOT empty! " +
          s"Please either delete or empty the directory."
      )
    }

    // create the output directories for the specific run
    dirsToBeCreated.foreach(createDir)

  }

  /** Create a directory at the provided location
    *
    * @param dir
    *   the full path where the directory should be created (incl. it's name)
    */
  private def createDir(dir: Path): Unit = {
    val dirFile = dir.toFile
    if (!dirFile.mkdirs() && !dirFile.exists())
      throw new FileHierarchyException(
        "The output directory path " + dir
          + " could not be created. Check pathname and permissions! Full path: " + dirFile.getAbsolutePath
      )
  }

  /** Deletes the temp folder of a provided [[ResultFileHierarchy]]
    *
    * @param outputFileHierarchy
    *   the [[ResultFileHierarchy]] which temp folder should be deleted
    */
  def deleteTmpDir(outputFileHierarchy: ResultFileHierarchy): Unit = {
    FileIOUtils.deleteRecursively(outputFileHierarchy.tmpDir)
  }

}
