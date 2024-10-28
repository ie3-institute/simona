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
        )

      val runOutputDirWithDate =
        "vn_simona_".concat(validOutputFileHierarchy.runStartTimeUTC)

      relativizePath(
        validOutputFileHierarchy.baseOutputDir
      ).toString shouldBe baseOutputDir
      relativizePath(
        validOutputFileHierarchy.runOutputDir
      ).toString shouldBe baseOutputDir + fileSeparator + runOutputDirWithDate
      relativizePath(
        validOutputFileHierarchy.tmpDir
      ).toString shouldBe baseOutputDir + fileSeparator + runOutputDirWithDate + fileSeparator + "tmp"
      relativizePath(
        validOutputFileHierarchy.configOutputDir
      ).toString shouldBe baseOutputDir + fileSeparator + runOutputDirWithDate + fileSeparator + "configs"
      relativizePath(
        validOutputFileHierarchy.rawOutputDataDir
      ).toString shouldBe baseOutputDir + fileSeparator + runOutputDirWithDate + fileSeparator + "rawOutputData"
      relativizePath(
        validOutputFileHierarchy.graphOutputDir
      ).toString shouldBe baseOutputDir + fileSeparator + runOutputDirWithDate + fileSeparator + "graphs"
      relativizePath(
        validOutputFileHierarchy.kpiOutputDir
      ).toString shouldBe baseOutputDir + fileSeparator + runOutputDirWithDate + fileSeparator + "kpi"

      relativizePath(
        validOutputFileHierarchy.rawOutputDataFilePaths(classOf[PvResult])
      ).toString shouldBe baseOutputDir + fileSeparator + runOutputDirWithDate + fileSeparator + "rawOutputData" + fileSeparator + "pref_pv_res_suff.csv"

    }
    "not write directories automatically on instantiation" in {} // todo

    "write directories automatically on instantiation when requested so" in {
      // delete file if they exist
      if (Files.exists(parentOutputFilePath))
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
          createDirs = true,
        )

      // check for existence of run output dir
      ResultFileHierarchy.runOutputDirExists(
        validOutputFileHierarchy
      ) shouldBe true

      // check for existence of other folders
      assert(
        Files.exists(new File(validOutputFileHierarchy.baseOutputDir).toPath)
      )
      assert(Files.exists(new File(validOutputFileHierarchy.tmpDir).toPath))
      assert(
        Files.exists(new File(validOutputFileHierarchy.configOutputDir).toPath)
      )
      assert(
        Files.exists(new File(validOutputFileHierarchy.rawOutputDataDir).toPath)
      )
      assert(
        Files.exists(new File(validOutputFileHierarchy.graphOutputDir).toPath)
      )
      assert(
        Files.exists(new File(validOutputFileHierarchy.kpiOutputDir).toPath)
      )

      // check if tmp directory can be deleted by output file hierarchy
      ResultFileHierarchy.deleteTmpDir(validOutputFileHierarchy)
      assert(!Files.exists(new File(validOutputFileHierarchy.tmpDir).toPath))

    }

    "should add a timestamp automatically on instantiation" in {} // todo

  }

  private def relativizePath(fullPath: String): Path = {
    new File(new File("").getAbsolutePath).toPath
      .relativize(new File(fullPath).toPath)
  }

  // todo output model path config compression should always be disabled -> test for this

}
