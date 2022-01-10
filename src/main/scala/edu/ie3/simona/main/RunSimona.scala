/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.main

import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.main.start.SimonaStarter
import edu.ie3.simona.sim.setup.SimonaSetup
import edu.ie3.util.scala.quantities.QuantityUtil

import java.util.Locale

/** Trait to be mixed in all implementations that should be used to run a simona
  * simulation. For a sample implemenation see [[RunSimonaStandalone]].
  *
  * @version 0.1
  * @since 01.07.20
  */
trait RunSimona[T <: SimonaSetup] extends LazyLogging {

  // timeout parameter
  protected implicit val timeout: Timeout

  def main(args: Array[String]): Unit = {
    Locale.setDefault(Locale.ENGLISH)

    /* The quantity library cannot handle scala's BigDecimal by default. Therefore, adjust the number system to use */
    QuantityUtil.adjustNumberSystem()

    RunPrinting.printOpener()
    logger.info(
      s"Starting SIMONA with interface '${getClass.getSimpleName.replaceAll("\\$", "")}'."
    )

    setup(args).foreach { case (simonaSetup, starter) =>
      starter.start(runAndExit(simonaSetup))
    }
  }

  /** Method to be implemented to setup everything that is necessary for a
    * sequence of simulations. This is by creating an instance of
    * [[SimonaSetup]] implementation
    *
    * @param args
    *   arguments provided by the command line
    * @return
    *   pairs of setup instance and a starter
    */
  protected def setup(args: Array[String]): Seq[(T, SimonaStarter)]

  /** Actually run the simona simulation using the provided [[SimonaSetup]] and
    * exit the program once done.
    *
    * @param simonaSetup
    *   the setup data that should be used
    * @param actorSystem
    *   the actor system to run SIMONA in
    */
  private[main] def runAndExit(
      simonaSetup: T
  )(actorSystem: ActorSystem): Unit = {
    run(simonaSetup, actorSystem)
    exit()
  }

  /** Actually run the simona simulation using the provided [[SimonaSetup]]
    *
    * @param simonaSetup
    *   the setup data that should be used
    * @param actorSystem
    *   the actor system to run SIMONA in
    */
  def run(simonaSetup: T, actorSystem: ActorSystem)(implicit
      timeout: Timeout
  ): Unit

  /** Printing goodbye message and exiting program
    */
  private def exit(): Unit = {
    RunPrinting.printGoodbye()

    Thread.sleep(
      1000
    ) // prevents cutting of the log when having a fast simulation
    System.exit(0)
  }
}
