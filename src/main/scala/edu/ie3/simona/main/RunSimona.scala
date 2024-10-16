/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.sim.setup.SimonaSetup
import edu.ie3.util.scala.quantities.QuantityUtil
import org.apache.pekko.util.Timeout

import java.nio.file.Path
import java.util.Locale
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/** Trait to be mixed in all implementations that should be used to run a simona
  * simulation. For a sample implementation see [[RunSimonaStandalone]].
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

    val simonaSetup = setup(args)

    val successful = run(simonaSetup)

    printGoodbye(successful, simonaSetup.logOutputDir)

    // prevents cutting of the log when having a fast simulation
    Thread.sleep(1000)

    System.exit(if (successful) 0 else 1)
  }

  // a fancy opener
  private def printOpener(): Unit = {
    logger.info(
      s"Starting SIMONA with interface '${getClass.getSimpleName.replaceAll("\\$", "")}'.\n" + "   _____ ______  _______  _   _____ \n  / ___//  _/  |/  / __ \\/ | / /   |\n  \\__ \\ / // /|_/ / / / /  |/ / /| |\n ___/ // // /  / / /_/ / /|  / ___ |\n/____/___/_/  /_/\\____/_/ |_/_/  |_|\n                                    "
    )
  }

  private def printGoodbye(
      successful: Boolean,
      outputPath: String = "",
  ): Unit = {
    val myWords = Array(
      "\"Vielleicht ist heute ein besonders guter Tag zum Sterben.\" - Worf (in Star Trek: Der erste Kontakt)",
      "\"Assimiliert das!\" - Worf (in Star Trek: Der erste Kontakt)",
      "\"Lebe lang und erfolgreich.\" - Gruppe von Vulkanier (in Star Trek: Der erste Kontakt)",
      "\"Ich bin der Anfang, das Ende, die Eine, die Viele ist. Ich bin die Borg.\" - Borg-Königin (in Star Trek: Der erste Kontakt)",
      "\"A horse! A horse! My kingdom for a horse!\" - King Richard III (in Shakespeare's Richard III, 1594)",
      "\"Und wenn du lange in einen Abgrund blickst, blickt der Abgrund auch in dich hinein\" - F. Nietzsche",
      "\"Before anything else, preparation is the key to success.\" - Alexander Graham Bell",
    )

    val rand = new Random
    val randIdx = rand.nextInt(myWords.length)
    logger.info(myWords(randIdx))
    logger.info("Goodbye!")

    if (!successful) {
      // to ensure that the link to the log is printed last
      Thread.sleep(1000)

      val path = Path.of(outputPath).resolve("simona.log").toUri

      logger.error(
        s"Simulation stopped due to the occurrence of an error! The full log can be found here: $path"
      )
    }
  }

  /** Method to be implemented to setup everything that is necessary for a
    * simulations. This is by creating an instance of [[SimonaSetup]]
    * implementation
    *
    * @param args
    *   arguments provided by the command line
    * @return
    *   the setup instance
    */
  def setup(args: Array[String]): T

  /** Actually run the simona simulation using the provided [[SimonaSetup]]
    *
    * @param simonaSetup
    *   the setup data that should be used
    * @return
    *   Whether the simualtion was successful or not
    */
  def run(simonaSetup: T): Boolean

}

object RunSimona {

  /** Reported back from the scheduler if an error occurred during the
    * simulation
    */
  final case class SimonaEnded(successful: Boolean)

}
