/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.helper

import org.apache.pekko.testkit.TestException
import org.testcontainers.utility.MountableFile

import java.io.File
import java.net.URL
import java.nio.file.{Path, Paths}

trait TestResourceHelper {

  protected def getResourcePath(resource: String): Path =
    Paths.get(getResource(resource).toURI)

  protected def getResourceFile(resource: String): File =
    new File(getResource(resource).getPath)

  /** Retrieve resource with the class' resource loader. In contrast to
    * [[org.testcontainers.utility.MountableFile#forClasspathResource(java.lang.String, java.lang.Integer)]],
    * this also works with paths relative to the current class (i.e. without
    * leading '/').
    * @param resource
    *   the resource directory or file path
    * @return
    *   a MountableFile to use with test containers
    */
  protected def getMountableFile(resource: String): MountableFile =
    MountableFile.forHostPath(Paths.get(getResource(resource).toURI))

  private def getResource(resource: String): URL =
    Option(getClass.getResource(resource))
      .getOrElse(
        throw TestException(
          "Resource '" + resource + "' was not found from " + getClass.toString
        )
      )

}
