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
  private val workingJar2 = s"$jarsDir/mock_ext_sim_2.jar"
  private val wrongImplementationJar =
    s"$jarsDir/mock_ext_sim-wrong_implementation.jar"
  private val emptyFileJar = s"$jarsDir/mock_ext_sim-empty_file.jar"
  private val missingServiceFileJar =
    s"$jarsDir/mock_ext_sim-missing_service_file.jar"

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
      files.size shouldBe 6

      files.count(file => fileSuffix(file.getName) == "jar") shouldBe 5

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

    "throw exception when the META-INF/service file is missing" in {
      val jar = getResource(missingServiceFileJar)
      assertThrows[ClassNotFoundException] {
        val file = Iterable[File](jar)
        ExtSimLoader.loadExtLink(file)
      }
    }

    "throw exception when service file is empty" in {
      val jar = getResource(emptyFileJar)
      assertThrows[ClassCastException] {
        val file = Iterable[File](jar)
        ExtSimLoader.loadExtLink(file)
      }
    }

    "throw exception when ExtLinkInterface is not implemented" in {
      val jar = getResource(wrongImplementationJar)
      assertThrows[ClassCastException] {
        val file = Iterable[File](jar)
        ExtSimLoader.loadExtLink(file)
      }
    }

    "load a proper jar correctly" in {
      val jar = getResource(workingJar)
      val file = Iterable[File](jar)
      val extSim = ExtSimLoader.loadExtLink(file)

      extSim should not be null
      extSim shouldBe an[ExtLinkInterface]
    }

    "load a multiple jars correctly" in {
      val jarOne = getResource(workingJar)
      val jarTwo = getResource(workingJar2)
      val extSim = ExtSimLoader.loadExtLink(Iterable(jarOne, jarTwo))

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
