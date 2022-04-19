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
import java.util.ServiceLoader

/** Finds and loads jars containing external simulations.
  */
object ExtSimLoader extends LazyLogging {

  private val extSimPath = "input" + java.io.File.separator + "ext_sim"

  // private val extLinkClassPath = "edu.ie3.simona.api.ExtLink"

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
  }

  def loadExtLink(myJar: Iterable[File]): ServiceLoader[ExtLinkInterface] = {
    val JarIterator = myJar.iterator
    val length = JarIterator.length
    val urls = Array.ofDim[java.net.URL](length)

    while (JarIterator.hasNext) {
      var index: Int = 0
      urls.update(index, JarIterator.next().toURI.toURL)
      index += 1
    }

    val classLoader = new URLClassLoader(urls, this.getClass.getClassLoader)

    val services = ServiceLoader.load(classOf[ExtLinkInterface], classLoader)
    services
  }
}
