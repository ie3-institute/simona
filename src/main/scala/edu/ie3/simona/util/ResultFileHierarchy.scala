/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.text.SimpleDateFormat
import com.typesafe.config.{ConfigRenderOptions, Config => TypesafeConfig}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.naming.{EntityPersistenceNamingStrategy, FileNamingStrategy,
}
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.exceptions.FileHierarchyException
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.io.result.ResultSinkType.Csv
import edu.ie3.simona.logging.logback.LogbackConfiguration
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import edu.ie3.util.io.FileIOUtils
import org.apache.commons.io.FilenameUtils._

import scala.jdk.OptionConverters.RichOptional

/** Represents the output directory where the results will be materialized. If
  * new directories are added please remember to add them to the dirsToBeCreated
  * Vector if they should be created. Otherwise they will not be created!
  *
  * @version 0.1
  * @since 12.01.20
  */
final case class ResultFileHierarchy(
    private val outputDir: String,
    private val simulationName: String,
    private val resultEntityPathConfig: ResultEntityPathConfig,
    private val configureLogger: String => Unit = LogbackConfiguration.default,
    private val addTimeStampToOutputDir: Boolean = true,
    private val createDirs: Boolean = false,
) extends LazyLogging {

  private val fileSeparator: String = File.separator

  val runStartTimeUTC: String =
    new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss").format(new java.util.Date())

  val baseOutputDir: String = buildBaseOutputDir

  val runOutputDir: String = buildRunOutputDir

  val configOutputDir: String =
    runOutputDir.concat(fileSeparator).concat("configs")

  val rawOutputDataDir: String =
    runOutputDir.concat(fileSeparator).concat("rawOutputData")

  val logOutputDir: String =
    runOutputDir.concat(fileSeparator).concat("log")

  val resultSinkType: ResultSinkType = resultEntityPathConfig.resultSinkType

  val resultEntitiesToConsider: Set[Class[_ <: ResultEntity]] =
    resultEntityPathConfig.resultEntitiesToConsider

  val rawOutputDataFilePaths: Map[Class[_ <: ResultEntity], String] = {
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
                fileSeparator,
              ),
            )
          )
          .toMap
      case _ =>
        Map.empty
    }
  }

  val graphOutputDir: String =
    runOutputDir.concat(fileSeparator).concat("graphs")

  val kpiOutputDir: String = runOutputDir.concat(fileSeparator).concat("kpi")

  val tmpDir: String = runOutputDir.concat(fileSeparator).concat("tmp")

  private val dirsToBeCreated: Vector[String] = Vector(
    baseOutputDir,
    runOutputDir,
    configOutputDir,
    rawOutputDataDir,
    graphOutputDir,
    kpiOutputDir,
    tmpDir,
    logOutputDir,
  )

  // needs to be the latest call because otherwise the values are null as they are not initialized yet
  if (createDirs)
    ResultFileHierarchy.createOutputDirectories(this)

  // needs to be done after dir creation
  configureLogger(logOutputDir)

  /** Builds the base output directory string
    *
    * @return
    *   the filepath string to the directory
    */
  private def buildBaseOutputDir: String = {

    // clean file string if necessary
    val cleanedBaseOutputDir = {
      val normalizedOutputDir = normalize(outputDir)
      (fileSeparator + "$").r.replaceAllIn(normalizedOutputDir, "")
    }

    // create base output dir if non-existent
    Paths.get(cleanedBaseOutputDir).toFile.getAbsolutePath
  }

  /** Builds the output directory string for this specific run
    *
    * @return
    */
  private def buildRunOutputDir: String = {
    val optionalSuffix =
      if (addTimeStampToOutputDir) s"_$runStartTimeUTC" else ""
    baseOutputDir
      .concat(fileSeparator)
      .concat(simulationName)
      .concat(optionalSuffix)
  }

}

object ResultFileHierarchy extends LazyLogging {

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
    *   the model class a file path should be build for
    * @param csvSink
    *   the csv sink type parameters
    * @param rawOutputDataDir
    *   the directory of the raw output data
    * @param fileSeparator
    *   the file separator to be used
    * @return
    *   an absolute file path as string for the provided model class incl. file
    *   name + extension
    */
  private def buildRawOutputFilePath(
      modelClass: Class[_ <: ResultEntity],
      csvSink: Csv,
      rawOutputDataDir: String,
      fileSeparator: String,
  ): String = {
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

    rawOutputDataDir
      .concat(fileSeparator)
      .concat(filename.toString)
      .concat(fileEnding)
  }

  /** Prepares the output directories to be ready to hold the output data. This
    * includes creating the run directory with all subsequent directories as
    * well as copying the simulation configuration to the output dir
    *
    * @param config
    *   the config of the current simulation
    * @param resultFileHierarchy
    *   the output file hierarchy of the current simulation
    */
  def prepareDirectories(
      config: TypesafeConfig,
      resultFileHierarchy: ResultFileHierarchy,
  ): Unit = {
    // create output directories if they are not present yet
    if (!runOutputDirExists(resultFileHierarchy))
      ResultFileHierarchy.createOutputDirectories(resultFileHierarchy)

    logger.info(
      "Processing configs for simulation: {}.",
      config.getString("simona.simulationName"),
    )

    val outFile =
      Paths.get(resultFileHierarchy.configOutputDir, "vn_simona.conf").toFile
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

  /** Checks if the directory of the current run already exists
    *
    * @param outputFileHierarchy
    *   the [[ResultFileHierarchy]] that holds information on the run directory
    *   path
    * @return
    *   true if it exists, false if not
    */
  def runOutputDirExists(outputFileHierarchy: ResultFileHierarchy): Boolean = {
    new File(outputFileHierarchy.runOutputDir).exists() && new File(
      outputFileHierarchy.runOutputDir
    ).listFiles().length > 0
  }

  /** Creates all output directories of the provided [[ResultFileHierarchy]]
    *
    * @param outputFileHierarchy
    *   the [[ResultFileHierarchy]] the directories should be created for
    */
  def createOutputDirectories(
      outputFileHierarchy: ResultFileHierarchy
  ): Unit = {
    // try to create base output dir
    val baseOutputDir = Paths.get(outputFileHierarchy.baseOutputDir)
    // / check for existence of the provided baseOutputDir, if not create it
    if (Files.exists(baseOutputDir) && baseOutputDir.toFile.isFile) {
      throw new FileHierarchyException(
        s"Provided base output path $baseOutputDir is a file and cannot be replaced with a directory!"
      )
    }

    // check if there is data inside the runOutputDir taking into account the provided FileHandling
    val runOutputDir = new File(outputFileHierarchy.runOutputDir)
    if (runOutputDir.exists() && runOutputDir.listFiles().length > 0) {
      // files inside the runOutputDir -> fail
      throw new FileHierarchyException(
        s"The runOutputDir ${outputFileHierarchy.runOutputDir} already exists and is NOT empty! " +
          s"Please either delete or empty the directory."
      )
    }

    // create the output directories for the specific run
    outputFileHierarchy.dirsToBeCreated.foreach(createDir)

  }

  /** Create a directory at the provided location
    *
    * @param dir
    *   the full path where the directory should be created (incl. it's name)
    */
  private def createDir(dir: String): Unit = {
    val dirFile = new File(dir)
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
