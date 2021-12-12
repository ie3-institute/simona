/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.ExtLinkInterface
import edu.ie3.simona.test.common.UnitSpec

import java.io.File

class ExtSimLoaderSpec extends UnitSpec {

  private val resourceDir = "ext-sim-loader"

  private val noJarsDir = s"$resourceDir/no-jars"
  private val jarsDir = s"$resourceDir/jars"

  private val workingJar = s"$jarsDir/mock_ext_sim.jar"
  private val wrongExtLinkJar = s"$jarsDir/mock_ext_sim-wrong_extlink.jar"
  private val missingExtLinkJar = s"$jarsDir/mock_ext_sim-missing_extlink.jar"

  // testing whether this test is complete
  "An ExtSimLoaderSpec " should {
    "have a test directory available" in {
      val dir = getResource(resourceDir)
      assert(dir.isDirectory, "Testing resource directory does not exist")
    }

    "have a directory with an irrelevant file" in {
      val dir = getResource(noJarsDir)
      assert(dir.isDirectory, "Directory with irrelevant file does not exist")

      val files = dir.listFiles().toVector
      files.size shouldBe 1

      val file = files.headOption.value
      assert(file.isFile)
      fileSuffix(file.getName) shouldBe "txt"
    }

    "have a directory with all relevant jars" in {
      val dir = getResource(jarsDir)
      assert(dir.isDirectory, "Directory with jars does not exist")

      val files = dir.listFiles().toVector
      files.size shouldBe 4

      files.count(file => fileSuffix(file.getName) == "jar") shouldBe 3

      files.count(file => fileSuffix(file.getName) == "txt") shouldBe 1
    }
  }

  // testing the actual class
  "An ExtSimLoader" should {
    "ignore irrelevant files" in {
      val dir = getResource(noJarsDir)
      val jars = ExtSimLoader.scanInputFolder(dir)

      jars shouldBe empty
    }

    "find all jars in directory" in {
      val dir = getResource(jarsDir)
      val jars = ExtSimLoader.scanInputFolder(dir)

      jars.size shouldBe 3

      jars.foreach { jar =>
        fileSuffix(jar.getName) shouldBe "jar"
      }
    }

    "throw exception when ExtLink is missing" in {
      val jar = getResource(missingExtLinkJar)
      assertThrows[ClassNotFoundException] {
        ExtSimLoader.loadExtLink(jar)
      }
    }

    "throw exception when ExtLink does not inherit from ExtLinkInterface" in {
      val jar = getResource(wrongExtLinkJar)
      assertThrows[ClassCastException] {
        ExtSimLoader.loadExtLink(jar)
      }
    }

    "load a proper jar correctly" in {
      val jar = getResource(workingJar)
      val extSim = ExtSimLoader.loadExtLink(jar)

      extSim should not be null
      extSim shouldBe an[ExtLinkInterface]
    }
  }

  private def getResource(name: String): File = {
    val path = getClass.getResource(name)
    path should not be null
    new File(path.getPath)
  }

  private def fileSuffix(fileName: String): String =
    if (fileName.contains("."))
      fileName.substring(fileName.lastIndexOf('.') + 1)
    else
      fileName
}
