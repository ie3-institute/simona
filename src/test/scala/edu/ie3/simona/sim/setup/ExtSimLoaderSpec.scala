/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.ExtLinkInterface
import edu.ie3.simona.test.common.UnitSpec

import java.io.File
import java.util.ServiceConfigurationError

class ExtSimLoaderSpec extends UnitSpec {

  private val resourceDir = "ext-sim-loader"

  private val noJarsDir = s"$resourceDir/_no-jars"
  private val jarsDir = s"$resourceDir/_jars"

  private val workingJar = s"$jarsDir/mock_ext_sim.jar"
  private val workingJar2 = s"$jarsDir/mock_ext_sim-2.jar"
  private val wrongImplementationJar =
    s"$jarsDir/mock_ext_sim-wrong_implementation.jar"
  private val emptyFileJar = s"$jarsDir/mock_ext_sim-empty_file.jar"
  private val missingServiceFileJar =
    s"$jarsDir/mock_ext_sim-missing_service_file.jar"
  private val twoImplementationJar =
    s"$jarsDir/mock_ext_sim-two_implementations.jar"

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
      files.size shouldBe 7

      files.count(file => fileSuffix(file.getName) == "jar") shouldBe 6

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

      jars.size shouldBe 6

      jars.foreach { jar =>
        fileSuffix(jar.getName) shouldBe "jar"
      }
    }

    "load no service if the META-INF/service file is missing" in {
      val jar = getResource(missingServiceFileJar)
      val extLink = ExtSimLoader.loadExtLink(jar)

      extLink.isEmpty shouldBe true
    }

    "load no service if service file is empty" in {
      val jar = getResource(emptyFileJar)
      val extLink = ExtSimLoader.loadExtLink(jar)

      extLink.isEmpty shouldBe true
    }

    "throw an exception when ExtLinkInterface is not implemented" in {
      val jar = getResource(wrongImplementationJar)
      assertThrows[ServiceConfigurationError] {
        ExtSimLoader.loadExtLink(jar)
      }
    }

    "load a proper jar correctly" in {
      val jar = getResource(workingJar)
      val jars = Iterable(jar)
      val extLinks = jars.flatMap(ExtSimLoader.loadExtLink)

      extLinks.size shouldBe 1

      extLinks.headOption.value should not be null
      extLinks.headOption.value shouldBe an[ExtLinkInterface]
    }

    "load multiple proper jars correctly" in {
      val jarOne = getResource(workingJar)
      val jarTwo = getResource(workingJar2)
      val jars = Iterable(jarOne, jarTwo)
      val extLinks = jars.flatMap(ExtSimLoader.loadExtLink)

      extLinks.size shouldBe 2

      extLinks.map { extLink =>
        extLink should not be null
        extLink shouldBe an[ExtLinkInterface]
      }
    }

    "load a jar with multiple ExtLinks" in {
      val jarOne = getResource(twoImplementationJar)
      val jars = Iterable(jarOne)
      val extLinks = jars.flatMap(ExtSimLoader.loadExtLink)

      extLinks.size shouldBe 1
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
