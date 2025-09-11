/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.deploy

import edu.ie3.simona.test.common.UnitSpec
import org.scalatest.Ignore

import java.io.File
import scala.io.{Codec, Source}
import scala.language.{existentials, postfixOps}
import scala.util.{Failure, Success, Using}

// this is secure functionality spec to ensure that the deployment script is altered if required in order to maintain
// its functionality
// IF THIS SPEC FAILS IT'S VERY LIKELY THAT YOU HAVE TO ALTER src/main/sh/deploy/run-simona-cmd.sh
@Ignore
class DeploySpec extends UnitSpec {

  private val runSimonaCmdSh = Using(
    Source.fromFile(
      System.getProperty(
        "user.dir"
      ) + File.separator + "src/main/sh/deploy/run-simona-cmd.sh"
    )
  )(_.getLines().mkString) match {
    case Failure(exception) =>
      fail("Cannot read src/main/sh/deploy/run-simona-cmd.sh!", exception)
    case Success(runCmdLines) => runCmdLines
  }

  "The sh/run-simona-cmd.sh" should {

    "contain the current build version number" in {

      // find substring in build.gradle
      val versionProps = Using(
        Source.fromFile(
          System.getProperty("user.dir") + File.separator + "version.properties"
        )(using Codec.UTF8)
      )(_.getLines().mkString) match {
        case Failure(exception)   => fail(exception)
        case Success(buildGradle) => buildGradle
      }

      val gradleVersionString =
        versionProps
          .substring(versionProps.indexOf("version.semver=") + 15)
          .trim

      // find substring of -cp <SIMONA.jar>
      val subStringStart = runSimonaCmdSh.indexOf("-cp ") + 4
      val subStringEnd = runSimonaCmdSh.substring(subStringStart).indexOf("\\")
      val cpArgString =
        runSimonaCmdSh.substring(subStringStart).substring(0, subStringEnd).trim

      // ensure cp arg contains current simona version
      assert(cpArgString.contains(gradleVersionString))

    }

    "contain an existing class with a main method as execution parameter" in {
      val classStartString = runSimonaCmdSh.indexOf("edu.ie3.simona")
      val classEndString =
        runSimonaCmdSh.substring(classStartString).indexOf("\\")
      val classString = runSimonaCmdSh
        .substring(classStartString)
        .substring(0, classEndString)
        .trim

      val runClass = scala.util.Try {
        Class.forName(classString)
      } match {
        case Failure(exception) =>
          fail(
            s"Cannot build class from execution class '$classString' in run-simona-cmd.sh!",
            exception,
          )
        case Success(clazz) => clazz
      }

      runClass.getMethods.count(_.getName.contains("main")) shouldBe 1

    }

  }

}
