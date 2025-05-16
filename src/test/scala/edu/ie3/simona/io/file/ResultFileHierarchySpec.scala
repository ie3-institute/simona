/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.file

import java.io.File
import java.nio.file.{Files, Path}
import edu.ie3.datamodel.models.result.system.PvResult
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.logging.logback.LogbackConfiguration
import edu.ie3.simona.test.common.{IOTestCommons, UnitSpec}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import edu.ie3.util.io.FileIOUtils
import org.scalatest.BeforeAndAfterAll

class ResultFileHierarchySpec
    extends UnitSpec
    with BeforeAndAfterAll
    with IOTestCommons {

  val mainDir: String = testTmpDir
  val parentOutputFilePath: Path = new File(mainDir).toPath
  val fileSeparator: String = File.separator
  val baseOutputDir: String =
    mainDir + fileSeparator + "output" + fileSeparator + "vn_simona"
  val runOutputDir = "vn_simona_test"

  override def afterAll(): Unit = {
    // delete created directories
    FileIOUtils.deleteRecursively(parentOutputFilePath)
  }

  "A valid OutputFileHierarchy" should {

    "return all expected file paths" in {
      val fileSeparator: String = File.separator

      val baseOutputDir = "output" + fileSeparator + "vn_simona"
      val runOutputDir = "vn_simona"
      val validOutputFileHierarchy =
        ResultFileHierarchy(
          baseOutputDir,
          runOutputDir,
          ResultEntityPathConfig(
            Set(classOf[PvResult]),
            ResultSinkType.Csv("csv", "pref", "suff"),
          ),
          configureLogger =
            LogbackConfiguration.default("INFO", Some("ERROR"))(_),
        )

      validOutputFileHierarchy.tmpDir.toString shouldBe validOutputFileHierarchy.runOutputDir.toString + fileSeparator + "tmp"
      validOutputFileHierarchy.configOutputDir.toString shouldBe validOutputFileHierarchy.runOutputDir.toString + fileSeparator + "configs"
      validOutputFileHierarchy.logOutputDir.toString shouldBe validOutputFileHierarchy.runOutputDir.toString + fileSeparator + "log"

      validOutputFileHierarchy
        .rawOutputDataFilePaths(classOf[PvResult])
        .toString shouldBe validOutputFileHierarchy.runOutputDir.toString + fileSeparator + "rawOutputData" + fileSeparator + "pref_pv_res_suff.csv"

    }

    "write directories automatically on instantiation when requested so" in {
      // delete file if they exist
      if Files.exists(parentOutputFilePath) then
        FileIOUtils.deleteRecursively(parentOutputFilePath)

      // init output file hierarchy with writing
      val validOutputFileHierarchy =
        ResultFileHierarchy(
          baseOutputDir,
          runOutputDir,
          ResultEntityPathConfig(
            Set(classOf[PvResult]),
            ResultSinkType.Csv("csv", "pref", "suff"),
          ),
          configureLogger =
            LogbackConfiguration.default("INFO", Some("ERROR"))(_),
        )

      // check for existence of run output dir
      ResultFileHierarchy.runOutputDirExists(
        validOutputFileHierarchy
      ) shouldBe true

      // check for existence of other folders
      assert(Files.exists(validOutputFileHierarchy.configOutputDir))
      assert(Files.exists(validOutputFileHierarchy.logOutputDir))
      assert(Files.exists(validOutputFileHierarchy.tmpDir))

      // check if tmp directory can be deleted by output file hierarchy
      ResultFileHierarchy.deleteTmpDir(validOutputFileHierarchy)
      assert(!Files.exists(validOutputFileHierarchy.tmpDir))

    }

    "should add a timestamp automatically on instantiation" in {} // todo

  }

  // todo output model path config compression should always be disabled -> test for this

}
