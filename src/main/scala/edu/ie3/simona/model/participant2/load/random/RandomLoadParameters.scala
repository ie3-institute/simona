/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load.random

import edu.ie3.simona.util.ParsableEnumeration

/** A consistent set of parameters, that are needed to describe a 'generalized
  * extreme value' probability density function. In general the GEV is described
  * by the three parameters "location", "scale" and "shape".
  *
  * @param k
  *   Shape parameter
  * @param my
  *   Location parameter
  * @param sigma
  *   Scale parameter
  */
final case class RandomLoadParameters(k: Double, my: Double, sigma: Double)

/** Enumeration to list all possible parameters
  */
case object RandomLoadParameters extends ParsableEnumeration {
  val K: Value = Value("k")
  val MY: Value = Value("my")
  val SIGMA: Value = Value("sigma")
}
