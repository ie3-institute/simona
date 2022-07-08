/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import org.scalatest.Tag

sealed trait TestTags
object TestTags {
  object SnailTest
      extends Tag("edu.ie3.simona.test.common.TestTags.SnailTest")
      with TestTags
}
