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
import scala.jdk.CollectionConverters._

/** Finds and loads jars containing external simulations.
  */
object ExtSimLoader extends LazyLogging {

  private val extSimPath = "input" + java.io.File.separator + "ext_sim"

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

  def loadExtLink(myJar: File): Option[ExtLinkInterface] = {
    val classLoader = new URLClassLoader(Array(myJar.toURI.toURL))
    val service = ServiceLoader
      .load(classOf[ExtLinkInterface], classLoader)
      .asScala

    service.size match {
      case 1 =>
        logger.info(
          s"Jar file ${myJar.getName} was loaded with one implementation."
        )
      case count =>
        logger.warn(
          s"Jar file ${myJar.getName} was loaded with $count implementations."
        )
    }
    service.headOption
  }
}
