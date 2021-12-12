/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import java.io.File

/** Common I/O information that should be used for tests e.g. default
  * directories etc.
  *
  * @version 0.1
  * @since 05.03.20
  */
trait IOTestCommons {

  protected val testTmpDir: String = System.getProperty(
    "user.dir"
  ) + File.separator + "test" + File.separator + "tmp_" + this.getClass.getSimpleName

  def createDir(dir: String): Boolean = new File(dir).mkdirs()

}
