/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.helper

import org.apache.pekko.testkit.TestException
import org.testcontainers.utility.MountableFile

import java.nio.file.Paths

trait TestContainerHelper {

  /** Retrieve resource with the class' resource loader. In contrast to
    * [[org.testcontainers.utility.MountableFile#forClasspathResource(java.lang.String, java.lang.Integer)]],
    * this also works with paths relative to the current class (i.e. without
    * leading '/').
    * @param resource
    *   the resource directory or file path
    * @return
    *   a MountableFile to use with test containers
    */
  def getMountableFile(resource: String): MountableFile = {
    Option(getClass.getResource(resource))
      .map(url => Paths.get(url.toURI))
      .map(MountableFile.forHostPath)
      .getOrElse(
        throw TestException(
          "Resource '" + resource + "' was not found from " + getClass.toString
        )
      )
  }
}
