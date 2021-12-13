/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.util.scala.quantities.{QuantityUtil => PSQuantityUtil}
import org.scalatest._
import org.scalatest.matchers.should

import java.util.Locale

trait CommonToolsSpec
    extends should.Matchers
    with QuantityMatchers
    with OptionValues
    with Inside
    with Inspectors
    with PrivateMethodTester
    with LazyLogging
    with TryValues {
  /* Set default locale in order to ensure proper number parsing - among others */
  Locale.setDefault(Locale.ENGLISH)

  /* The quantity library cannot handle scala's BigDecimal by default. Therefore, adjust the number system to use */
  PSQuantityUtil.adjustNumberSystem()
}
