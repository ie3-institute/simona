/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.numerics.pow
import com.typesafe.scalalogging.LazyLogging
import squants.Each

import java.util.UUID

/** Provides methods to calculate the branch and phase-to-ground susceptance as
  * well as conductance of a grid asset to be represented by π equivalent
  * circuit.
  */
trait PiEquivalentCircuit extends LazyLogging {

  def uuid: UUID

  /** Resistance (real part) of the branch impedance in p.u.
    */
  protected def r: squants.Dimensionless

  private val rVal: Double = r.toEach

  /** Reactance (imaginary part) of the branch impedance in p.u.
    */
  protected def x: squants.Dimensionless

  private val xVal: Double = x.toEach

  /** Conductance (real part) of the TOTAL phase-to-ground admittance in p.u.
    */
  protected def g: squants.Dimensionless

  private val gVal: Double = g.toEach

  /** Susceptance (imaginary part) of the TOTAL phase-to-ground admittance in
    * p.u.
    */
  protected def b: squants.Dimensionless

  private val bVal: Double = b.toEach

  /** Computes the branch conductance of the grid element. Formula:
    *
    * {{{
    * y = 1 / (r + j*x)
    *   = (r - j*x) / (r² + x²)
    *   = (r / (r² + x²)) + j*(-x / (r² + x²))
    *   -> g = r / (r² + x²)
    * }}}
    *
    * @return
    *   branch conductance g_ij between node i and j of the element in p.u.
    */
  def gij(): squants.Dimensionless = {
    val gijVal: Double = {
      if rVal == 0 then 0
      else if xVal == 0 then 1 / rVal
      else rVal / (pow(rVal, 2) + pow(xVal, 2))
    }
    Each(gijVal)
  }

  /** Computes the branch susceptance of the grid element.
    *
    * Formula:
    * {{{
    * y = 1 / (r + j*x)
    *   = (r - j*x) / (r² + x²)
    *   = (r / (r² + x²)) + j*(-x / (r² + x²))
    *   -> b = -x / (r² + x²)
    * }}}
    *
    * @return
    *   branch susceptance b_ij between node i and j of the element in p.u.
    */
  def bij(): squants.Dimensionless = {
    val bijVal = {
      if xVal == 0 then 0
      else if rVal == 0 then -1 / xVal
      else -xVal / (pow(rVal, 2) + pow(xVal, 2))
    }
    Each(bijVal)
  }

  /** "Computes" the TOTAL phase-to-ground conductance of the grid element.
    *
    * @return
    *   phase-to-ground conductance g_0 in p.u.
    */
  def g0(): squants.Dimensionless = g

  /** "Computes" the TOTAL phase-to-ground susceptance of the grid element.
    *
    * @return
    *   phase-to-ground susceptance b_0 in p.u.
    */
  def b0(): squants.Dimensionless = {
    Each(bVal)
  }

  /** Simple method that displays a warning if the provided values seem
    * unreasonable high as this might cause trouble in further use. Note that,
    * for now, only 10 p.u. are used due to the lack of better thresholds. These
    * thresholds should be adapted in the future if more information are
    * available.
    *
    * @param modelType
    *   optional model type to improve warning output
    */
  final def piEquivalentSanityCheck(
      modelType: String = "model"
  ): Unit = {
    if rVal > 10 | xVal > 10 | bVal > 10 | gVal > 10 then
      logger.warn(
        s"PiEquivalent parameters for $modelType with uuid " +
          s"$uuid seem to be unreasonable. Values are r: {}, x: {}, g: {}, b: {}",
        r,
        x,
        g,
        b,
      )
  }

}
