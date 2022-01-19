/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.api.ExtLinkInterface

import java.io.{File, IOException}
import java.net.URLClassLoader

/** Finds and loads jars containing external simulations.
  */
object ExtSimLoader extends LazyLogging {

  private val extSimPath = "input" + java.io.File.separator + "ext_sim"

  private val extLinkClassPath = "edu.ie3.simona.api.ExtLink"

  def getStandardDirectory: File = {
    val workingDir = new File(System.getProperty("user.dir"))
    if (!workingDir.isDirectory)
      throw new IOException("Error when accessing working directory.")

    new File(workingDir, extSimPath)
  }

  def scanInputFolder(
      extSimDir: File = getStandardDirectory
  ): Iterable[File] = {
    if (!extSimDir.isDirectory) {
      logger.warn(
        s"External simulation directory ${extSimDir.getPath} does not exist or is not a directory, no external simulation loaded."
      )
      return Iterable.empty
    }

    val allowedExtensions = Seq("jar")

    extSimDir
      .listFiles()
      .filter { file =>
        val name = file.getName
        file.canRead &&
        name.contains('.') &&
        allowedExtensions.contains(
          name.substring(name.lastIndexOf('.') + 1).toLowerCase
        )
      }
      .toIterable
  }

  def loadExtLink(myJar: File): ExtLinkInterface = {
    val classLoader = new URLClassLoader(
      Array(myJar.toURI.toURL),
      this.getClass.getClassLoader
    )
    val classToLoad = Class.forName(extLinkClassPath, true, classLoader)
    classToLoad.newInstance match {
      case extSim: ExtLinkInterface =>
        extSim
      case other =>
        throw new ClassCastException(
          s"$extLinkClassPath in loaded jar ${myJar.getPath} is of wrong type ${other.getClass.getSimpleName}"
        )
    }
  }
}
