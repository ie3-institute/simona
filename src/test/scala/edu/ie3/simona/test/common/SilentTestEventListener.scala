/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import org.apache.pekko.event.Logging.{Debug, Error, Info, Warning, simpleName}
import org.apache.pekko.testkit.TestEventListener
import com.typesafe.scalalogging.LazyLogging

/** Does the same as TestEventListener but without polluting console logs. If
  * the log should be enabled for debug purposes adapt resources/log4j2-test.xml
  * and set '<AppenderRef ref="Console" level="off"/>' level to the prefered log
  * level
  */
class SilentTestEventListener extends TestEventListener with LazyLogging {

  override def print(event: Any): Unit =
    event match {
      case e: Error   => logger.error(e.logSource + ": {}", e.message)
      case e: Warning => logger.warn(e.logSource + ": {}", e.message)
      case e: Info    => logger.info(e.logSource + ": {}", e.message)
      case e: Debug   => logger.debug(e.logSource + ": {}", e.message)
      case e =>
        warning(
          Warning(
            simpleName(this),
            this.getClass,
            "received unexpected event of class " + e.getClass + ": " + e,
          )
        )
    }

}
