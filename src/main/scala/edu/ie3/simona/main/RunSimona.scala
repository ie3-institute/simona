/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.sim.setup.SimonaSetup
import edu.ie3.util.scala.quantities.QuantityUtil
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.pattern.gracefulStop
import org.apache.pekko.util.Timeout

import java.util.Locale
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/** Trait to be mixed in all implementations that should be used to run a simona
  * simulation. For a sample implemenation see [[RunSimonaStandalone]].
  *
  * @version 0.1
  * @since 01.07.20
  */
trait RunSimona[T <: SimonaSetup] extends LazyLogging {

  // timeout parameter
  implicit val timeout: Timeout
  implicit lazy val timeoutDuration: FiniteDuration = timeout.duration

  def main(args: Array[String]): Unit = {
    Locale.setDefault(Locale.ENGLISH)

    /* The quantity library cannot handle scala's BigDecimal by default. Therefore, adjust the number system to use */
    QuantityUtil.adjustNumberSystem()

    printOpener()

    setup(args).foreach(run)

    printGoodbye()

    Thread.sleep(
      1000
    ) // prevents cutting of the log when having a fast simulation
    System.exit(0)
  }

  def shutdownGracefully(
      simonaSim: ActorRef
  )(implicit timeout: FiniteDuration): Future[Boolean] = {
    gracefulStop(simonaSim, timeout)
  }

  // a fancy opener
  protected def printOpener(): Unit = {
    logger.info(
      s"Starting SIMONA with interface '${getClass.getSimpleName.replaceAll("\\$", "")}'.\n" + "   _____ ______  _______  _   _____ \n  / ___//  _/  |/  / __ \\/ | / /   |\n  \\__ \\ / // /|_/ / / / /  |/ / /| |\n ___/ // // /  / / /_/ / /|  / ___ |\n/____/___/_/  /_/\\____/_/ |_/_/  |_|\n                                    "
    )
  }

  def printGoodbye(): Unit = {
    val myWords = Array(
      "\"Vielleicht ist heute ein besonders guter Tag zum Sterben.\" - Worf (in Star Trek: Der erste Kontakt)",
      "\"Assimiliert das!\" - Worf (in Star Trek: Der erste Kontakt)",
      "\"Lebe lang und erfolgreich.\" - Gruppe von Vulkanier (in Star Trek: Der erste Kontakt)",
      "\"Ich bin der Anfang, das Ende, die Eine, die Viele ist. Ich bin die Borg.\" - Borg-Königin (in Star Trek: Der erste Kontakt)",
      "\"A horse! A horse! My kingdom for a horse!\" - King Richard III (in Shakespeare's Richard III, 1594)"
    )

    val rand = new Random
    val randIdx = rand.nextInt(myWords.length)
    logger.info(myWords(randIdx))
    logger.info("Goodbye!")
  }

  /** Method to be implemented to setup everything that is necessary for a
    * sequence of simulations. This is by creating an instance of
    * [[SimonaSetup]] implementation
    *
    * @param args
    *   arguments provided by the command line
    * @return
    *   the setup instances
    */
  def setup(args: Array[String]): Seq[T]

  /** Actually run the simona simulation using the provided [[SimonaSetup]]
    *
    * @param simonaSetup
    *   the setup data that should be used
    */
  def run(simonaSetup: T): Unit

}
