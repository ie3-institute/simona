/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.test.matchers.{
  DoubleMatchers,
  QuantityMatchers,
  SquantsMatchers,
}
import edu.ie3.util.scala.quantities.{QuantityUtil => PSQuantityUtil}
import org.apache.pekko.actor.testkit.typed.scaladsl.LogCapturing
import org.scalatest._
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.Locale

/** Base class to be used with all scala unit tests. All data that should be
  * commonly available to all unit tests should be placed here instead of mixing
  * a lot of traits together. See
  * http://www.scalatest.org/user_guide/defining_base_classes
  *
  * @version 0.1
  * @since 2019-09-08
  */
trait UnitSpec
    extends should.Matchers
    with QuantityMatchers
    with SquantsMatchers
    with DoubleMatchers
    with AnyWordSpecLike
    with LogCapturing
    with OptionValues
    with Inside
    with Inspectors
    with PrivateMethodTester
    with TableDrivenPropertyChecks
    with LazyLogging
    with TryValues
    with AppendedClues {
  /* Set default locale in order to ensure proper number parsing - among others */
  Locale.setDefault(Locale.ENGLISH)

  /* The quantity library cannot handle scala's BigDecimal by default. Therefore, adjust the number system to use */
  PSQuantityUtil.adjustNumberSystem()
}
