/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import java.util.UUID

import breeze.numerics.pow
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.util.quantities.PowerSystemUnits._
import javax.measure.quantity.Dimensionless
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

/** Provides methods to calculate the branch and phase-to-ground susceptance as
  * well as conductance of a grid asset to be represented by π equivalent
  * circuit.
  */
trait PiEquivalentCircuit extends LazyLogging {

  def uuid: UUID

  /** Resistance (real part) of the branch impedance in p.u.
    */
  protected def r: ComparableQuantity[Dimensionless]

  private val rVal: Double = r.to(PU).getValue.doubleValue()

  /** Reactance (imaginary part) of the branch impedance in p.u.
    */
  protected def x: ComparableQuantity[Dimensionless]

  private val xVal: Double = x.to(PU).getValue.doubleValue()

  /** Conductance (real part) of the TOTAL phase-to-ground admittance in p.u.
    */
  protected def g: ComparableQuantity[Dimensionless]

  private val gVal: Double = g.to(PU).getValue.doubleValue()

  /** Susceptance (imaginary part) of the TOTAL phase-to-ground admittance in
    * p.u.
    */
  protected def b: ComparableQuantity[Dimensionless]

  private val bVal: Double = b.to(PU).getValue.doubleValue()

  /** Computes the branch conductance of the grid element. Formula: y = 1 / (r +
    * j*x)
    * = (r - j*x) / (r² + x²)
    * = (r / (r² + x²)) + j*(-x / (r² + x²))
    * -> g = r / (r² + x²)
    *
    * @return
    *   branch conductance g_ij between node i and j of the element in p.u.
    */
  protected def gij(): ComparableQuantity[Dimensionless] = {
    val gijVal: Double = {
      if (rVal == 0) 0
      else if (xVal == 0) 1 / rVal
      else rVal / (pow(rVal, 2) + pow(xVal, 2))
    }
    Quantities.getQuantity(gijVal, PU)
  }

  /** Computes the branch susceptance of the grid element.
    *
    * Formula: y = 1 / (r + j*x)
    * = (r - j*x) / (r² + x²)
    * = (r / (r² + x²)) + j*(-x / (r² + x²))
    * -> b = -x / (r² + x²)
    *
    * @return
    *   branch susceptance b_ij between node i and j of the element in p.u.
    */
  protected def bij(): ComparableQuantity[Dimensionless] = {
    val bijVal = {
      if (xVal == 0) 0
      else if (rVal == 0) -1 / xVal
      else -xVal / (pow(rVal, 2) + pow(xVal, 2))
    }
    Quantities.getQuantity(bijVal, PU)
  }

  /** "Computes" the TOTAL phase-to-ground conductance of the grid element.
    *
    * @return
    *   phase-to-ground conductance g_0 in p.u.
    */
  protected def g0(): ComparableQuantity[Dimensionless] = {
    Quantities.getQuantity(gVal, PU)
  }

  /** "Computes" the TOTAL phase-to-ground susceptance of the grid element.
    *
    * @return
    *   phase-to-ground susceptance b_0 in p.u.
    */
  protected def b0(): ComparableQuantity[Dimensionless] = {
    Quantities.getQuantity(bVal, PU)
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
  protected final def piEquivalentSanityCheck(
      modelType: String = "model"
  ): Unit = {
    if (rVal > 10 | xVal > 10 | bVal > 10 | gVal > 10)
      logger.warn(
        s"PiEquivalent parameters for $modelType with uuid " +
          s"$uuid seem to be unreasonable. Values are r: {}, x: {}, g: {}, b: {}",
        r,
        x,
        g,
        b
      )
  }

}
